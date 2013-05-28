{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}

module Dang.TypeChecker.CheckKinds where

import Dang.IO
import Dang.ModuleSystem.Interface (HasInterface,getKinds)
import Dang.ModuleSystem.QualName
import Dang.Monad
import Dang.Syntax.AST
import Dang.TypeChecker.Env
import Dang.TypeChecker.Monad
import Dang.TypeChecker.Types
import Dang.TypeChecker.Unify as Types
import Dang.TypeChecker.Vars
import Dang.Utils.Location (at,unLoc,getLoc)
import Dang.Utils.Pretty
import Dang.Variables (sccToList,sccFreeNames)

import Control.Applicative ((<$>),(<*>))
import Control.Monad (foldM,mapAndUnzipM,replicateM)
import Data.Typeable (Typeable)
import qualified Data.Set as Set


-- Kind Variables --------------------------------------------------------------

freshKindVar :: TC Kind
freshKindVar  = freshVar setSort


-- Errors ----------------------------------------------------------------------

data KindError = KindError String
    deriving (Typeable,Show)

instance Exception KindError

-- | Raise a kind-checking error.
kindError :: String -> TC a
kindError  = raiseE . KindError


-- Kind Assumptions ------------------------------------------------------------

type KindAssumps = Assumps Kind

-- | Add the kind assumptions provided by an interface.
interfaceAssumps :: HasInterface iset => iset -> KindAssumps
interfaceAssumps  = foldl step emptyAssumps . getKinds
  where
  step env (qa,k) = addAssump qa (Assump Nothing k) env

-- | Find a kind in the set of kind assumptions.
kindAssump :: QualName -> KindAssumps -> TC Kind
kindAssump qn env = case lookupAssump qn env of
  Just a  -> return (aData a)
  Nothing -> unboundIdentifier qn

assume :: QualName -> Kind -> KindAssumps -> KindAssumps
assume qn k = addAssump qn (Assump Nothing k)

-- | Assume that a constructor has the given kind.
addKindAssump :: KindAssumps -> QualName -> Kind -> TC KindAssumps
addKindAssump env qn k = do
  logInfo ("  Assuming: " ++ pretty qn ++ " :: " ++ pretty k)
  return (assume qn k env)

-- | Add a group of primitive types to the kind assumptions.
addPrimTypes :: Namespace -> KindAssumps -> [PrimType] -> TC KindAssumps
addPrimTypes ns = foldM (addPrimType ns)

-- | Add the kind of a primitive type to the set of assumptions.
addPrimType :: Namespace -> KindAssumps -> PrimType -> TC KindAssumps
addPrimType ns env pty = addKindAssump env name kind
  where
  name = primName ns (primTypeName pty)
  kind = primTypeKind pty


-- Kind Checking ---------------------------------------------------------------

-- | Check the kinds of all type usages in a Module.  Return a Module that
-- contains all it's declarations with fixed kinds.
kcModule :: HasInterface iset => iset -> Module -> TC Module
kcModule iset m = do
  logInfo ("Checking module: " ++ pretty (modName m))
  let ns = modNamespace m
  env        <- addPrimTypes ns (interfaceAssumps iset) (modPrimTypes m)
  logDebug (show env)
  (env',ds') <- kcDataDecls ns env (modDatas m)
  pts'       <- mapM (kcPrimTerm env')    (modPrimTerms m)
  ts'        <- mapM (kcTypedDecl env')   (modTyped m)
  us'        <- mapM (kcUntypedDecl env') (modUntyped m)

  return m
    { modPrimTerms = pts'
    , modTyped     = ts'
    , modUntyped   = us'
    , modDatas     = ds'
    }

-- | Perform kind inference on a group of data declarations, augmenting an
-- environment with their kinds.
kcDataDecls :: Namespace -> KindAssumps -> [DataDecl]
            -> TC (KindAssumps,[DataDecl])
kcDataDecls ns env0 = foldM step (env0, []) . sccFreeNames ns
  where
  step (env,cs) scc = do
    let ds = sccToList scc
    ks <- mapM dataGenKind ds
    let ext e (d,k) = assume (qualName ns (dataName d)) k e
        dks         = zip ds ks
        envVar      = foldl ext env dks

    kds <- mapM (uncurry (kcDataDecl ns envVar)) dks

    let add e (k,d) = do
          k' <- applySubst k
          let qn = qualName ns (dataName d)
          logInfo ("  " ++ pretty qn ++ " :: " ++ pretty k')
          return (assume qn k' e)
    env' <- foldM add env kds
    return (env', map snd kds ++ cs)

-- | Kind check the constructors of a data declaration, returning a modified
-- kind-checking environment with the inferred kind.
kcDataDecl :: Namespace -> KindAssumps -> DataDecl -> Kind
           -> TC (Kind,DataDecl)
kcDataDecl ns env d gen = do
  let qn = qualName ns (dataName d)
  logInfo ("Checking: " ++ pretty qn)

  let env' = addAssump qn (Assump Nothing gen) env
  gs' <- mapM (kcQualConstrGroup qn gen env') (dataGroups d)

  k <- applySubst gen
  let d' = d { dataKind = k, dataGroups = gs' }
  return (k, d')

-- | General kind for a data declaration.
dataGenKind :: DataDecl -> TC Kind
dataGenKind d = mk `fmap` replicateM (dataArity d) freshKindVar
  where
  mk = foldr karrow kstar

-- | Check a qualified group of constructors.
kcQualConstrGroup :: QualName -> Kind -> KindAssumps -> Forall ConstrGroup
                  -> TC (Forall ConstrGroup)
kcQualConstrGroup qn gen env qcg = do
  let ps = forallParams qcg
      cg = forallData qcg
      ty = foldl TApp (TCon qn) (groupArgs cg)
  logInfo ("  Group: " ++ pretty ty)
  vars    <- replicateM (length ps) freshKindVar
  let step e (p,v) = addAssump (simpleName (paramName p)) (Assump Nothing v) e
      env'         = foldl step env (zip ps vars)
  (k,cg') <- kcConstrGroup env' cg
  logInfo ("    :: " ++ pretty k)
  unify gen k
  return qcg { forallData = cg' }

-- | Check a group of constructors, returning the inferred kind.
kcConstrGroup :: KindAssumps -> ConstrGroup -> TC (Kind,ConstrGroup)
kcConstrGroup env cg = do
  (ks,tys') <- mapAndUnzipM (inferKind env) (groupArgs cg)
  let k = foldr karrow kstar ks
  cs'     <- mapM (kcConstr env) (groupConstrs cg)
  let cg' = cg
        { groupArgs    = tys'
        , groupConstrs = cs'
        }
  return (k,cg')

-- | Check the fields of a constructor, unifying all of them to *.
kcConstr :: KindAssumps -> Constr -> TC Constr
kcConstr env c = do
  fs' <- mapM step (constrFields c)
  return c { constrFields = fs' }
  where
  step ty = do
    (k,ty') <- inferKind env ty
    logInfo ("  " ++ pretty ty' ++ " :: " ++ pretty k)
    unify kstar k
    return ty'


-- | Augment a kind-checking environment, adding the type variables of the data
-- declaration.
dataVars :: [TParam Kind] -> KindAssumps -> TC KindAssumps
dataVars ps env0 = foldM step env0 ps
  where
  step env p = do
    var <- freshVarFromTParam p
    let assump = Assump { aBody = Nothing, aData = var }
        name   = simpleName (paramName p)
    return (addAssump name assump env)

-- | Check the kind of a primitive term.  This should contain no variables, so
-- it's basically a sanity check.
kcPrimTerm :: KindAssumps -> PrimTerm -> TC PrimTerm
kcPrimTerm env pt = do
  qt <- kcTypeSig env (primTermType pt)
  return pt { primTermType = qt }

-- | Check the kind of the type of a declaration, then the kinds of any type
-- usages inside the term structure.
kcTypedDecl :: KindAssumps -> TypedDecl -> TC TypedDecl
kcTypedDecl env d = do
  qt <- kcTypeSig env (typedType d)
  b  <- kcMatch env (typedBody d)
  return d
    { typedType = qt
    , typedBody = b
    }

-- | Check the kinds of any types used within the body of an untyped
-- declaration.
kcUntypedDecl :: KindAssumps -> UntypedDecl -> TC UntypedDecl
kcUntypedDecl env d = do
  b <- kcMatch env (untypedBody d)
  return d { untypedBody = b }

-- | Introduce kind variables for all type variables, and kind check a type
-- signature.
kcTypeSig :: KindAssumps -> Scheme -> TC Scheme
kcTypeSig env qt = introType env qt $ \ env' ps qt' -> do
  logInfo ("Checking Type: " ++ pretty qt)

  let ty = qualData qt'
  (tyk,ty') <- inferKind env' ty
  unify kstar tyk

  let cxt = qualCxt qt'
  cxt' <- kcContext env' cxt

  return (quantify (map fixKind ps) (Qual cxt' ty'))

kcContext :: KindAssumps -> Context -> TC Context
kcContext env cxt = Set.fromList `fmap` mapM (kcConstraint env) (Set.toList cxt)

kcConstraint :: KindAssumps -> Constraint -> TC Constraint
kcConstraint env c = do
  (ck,c') <- inferKind env c
  unify kcxt ck
  return c'

-- | Check the kind structure of any types that show up in terms.
kcTerm :: KindAssumps -> Term -> TC Term
kcTerm env tm = case tm of
  Abs m       -> Abs  <$> kcMatch env m
  Case e m    -> Case <$> kcTerm env e <*> kcMatch env m
  Let ts us b -> Let  <$> mapM (kcTypedDecl env) ts
                      <*> mapM (kcUntypedDecl env) us
                      <*> kcTerm env b
  App f xs    -> App  <$> kcTerm env f <*> mapM (kcTerm env) xs
  Local{}     -> return tm
  Global{}    -> return tm
  Lit{}       -> return tm
  TLoc ltm    -> (TLoc . (`at` getLoc ltm)) `fmap` kcTerm env (unLoc ltm)

-- | Check the kind structure of any types hiding under a binder.
kcMatch :: KindAssumps -> Match -> TC Match
kcMatch env m = case m of
  MTerm t       -> MTerm    <$> kcTerm  env t
  MSplit l r    -> MSplit   <$> kcMatch env l <*> kcMatch env r
  MPat p m'     -> MPat p   <$> kcMatch env m'
  MGuard p e m' -> MGuard p <$> kcTerm env e  <*> kcMatch env m'
  MFail         -> return MFail

-- | Infer the kind of a type, fixing up internal kinds while we're at it.
inferKind :: KindAssumps -> Type -> TC (Kind,Type)
inferKind env ty = case ty of

  TApp l r -> do
    (lk,l') <- inferKind env l
    (rk,r') <- inferKind env r
    a       <- freshKindVar
    unify (rk `karrow` a) lk
    a'      <- applySubst a
    return (a',TApp l' r')

  -- Infix constructors should have their kind in the environment already, so
  -- it's just a case of unifying the arguments and the kind of the constructor
  -- produce a result.
  TInfix n l r -> do
    (lk,l') <- inferKind env l
    (rk,r') <- inferKind env r
    nk      <- kindAssump n env
    res     <- freshKindVar
    unify nk (lk `karrow` rk `karrow` res)
    res'    <- applySubst res
    return (res',TInfix n l' r')

  -- Constructors should just have their kinds already specified in the
  -- environment.  There's no need to check them, as they should only be
  -- constructed by the compiler.
  TCon n -> do
    k <- kindAssump n env
    return (k,ty)

  TVar tv -> do
    (k',tv') <- inferKindTVar env tv
    return (k', TVar tv')

inferKindTVar :: KindAssumps -> TVar Kind -> TC (Kind,TVar Kind)
inferKindTVar env tv = case tv of

  -- The idea here is that a variable might already have a kind associated with
  -- it through the environment, or that if it's a variable, that it might
  -- already have been unified with something concrete.
  UVar p -> do
    k' <- kindAssump (simpleName (paramName p)) env
    return (k', UVar p { paramKind = k' })

  GVar p -> do
    k' <- kindAssump (simpleName (paramName p)) env
    return (k', GVar p { paramKind = k' })


-- Type Helpers ----------------------------------------------------------------

-- | Introduce new kind variables for each quantified type variable.  Give to a
-- continuation, an environment that contains those variables, and a type with
-- them instantiated.
introType :: KindAssumps -> Scheme
          -> (KindAssumps -> [TParam Kind] -> Qual Type -> TC b)
          -> TC b
introType env (Forall ps qt) k = withVarIndex (length ps) $ do
  ps'  <- mapM freshTParam ps
  qt'  <- freshInst (Forall ps' qt)
  env' <- foldM (\e (q,t) -> addKindAssump e q t) env
      [ (simpleName (paramName p), paramKind p) | p <- ps' ]
  k env' ps' qt'

-- | Given a type parameter, assign it a fresh kind variable.
freshTParam :: TParam Kind -> TC (TParam Kind)
freshTParam p = do
  k <- freshKindVar
  return p { paramKind = k }

-- | Turn kind variables into stars.
fixKind :: TParam Kind -> TParam Kind
fixKind p = p { paramKind = Types.apply s k }
  where
  k = paramKind p
  s = unboundSubst [ (paramIndex v,kstar) | v <- Set.toList (typeVars k) ]
