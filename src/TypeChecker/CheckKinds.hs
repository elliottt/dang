{-# LANGUAGE DeriveDataTypeable #-}

module TypeChecker.CheckKinds where

import Dang.IO
import Dang.Monad
import ModuleSystem.Interface (HasInterface,getKinds)
import Pretty
import QualName
import Syntax.AST
import TypeChecker.Env
import TypeChecker.Monad
import TypeChecker.Types
import TypeChecker.Unify as Types
import Variables (sccToList,sccFreeNames)

import Control.Applicative ((<$>),(<*>))
import Control.Monad (foldM)
import Data.Typeable (Typeable)
import qualified Data.Set as Set


-- Utilities -------------------------------------------------------------------

freshKindVar :: TC Kind
freshKindVar  = freshVar setSort

data KindError = KindError String
    deriving (Typeable,Show)

instance Exception KindError

kindError :: String -> TC a
kindError  = raiseE . KindError

type KindAssumps = Assumps Kind

interfaceAssumps :: HasInterface iset => iset -> KindAssumps
interfaceAssumps  = foldl step emptyAssumps . getKinds
  where
  step env (qa,k) = addAssump qa (Assump Nothing k) env

kindAssump :: QualName -> KindAssumps -> TC Kind
kindAssump qn env = case lookupAssump qn env of
  Just a  -> return (aData a)
  Nothing -> unboundIdentifier qn

addKindAssump :: KindAssumps -> QualName -> Kind -> TC KindAssumps
addKindAssump env qn k = do
  logInfo ("  Assuming: " ++ pretty qn ++ " :: " ++ pretty k)
  return (addAssump qn (Assump Nothing k) env)

addPrimTypes :: Namespace -> KindAssumps -> [PrimType] -> TC KindAssumps
addPrimTypes ns = foldM (addPrimType ns)

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
kcDataDecls ns env0 = foldM step (env0, [])
  where
  step (env,ds) d = do
    (env',d') <- kcDataDecl ns env d
    return (env', d':ds)

-- | Kind check the constructors of a data declaration, returning a modified
-- kind-checking environment with the inferred kind.
kcDataDecl :: Namespace -> KindAssumps -> DataDecl -> TC (KindAssumps,DataDecl)
kcDataDecl ns env d = do
  let name = qualName ns (dataName d)
  logInfo ("Checking: " ++ pretty name)

  res <- freshKindVar
  let assump = Assump { aBody = Nothing, aData = res }

  let Forall ps cs = dataConstrs d
  declEnv <- dataVars ps (addAssump name assump env)

  cs' <- mapM (kcConstr declEnv) cs

  undefined

kcConstr :: KindAssumps -> Constr -> TC Constr
kcConstr env c = do
  undefined


-- | Augment a kind-checking environment, adding the type variables of the data
-- declaration.
dataVars :: [TParam] -> KindAssumps -> TC KindAssumps
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
kcTypeSig env qt = introType env qt $ \ env' ty -> do
  logInfo ("Checking Type: " ++ pretty ty)
  logDebug (show (typeVars ty))
  (tyk,ty') <- inferKind env' ty
  unify kstar tyk
  return (quantifyAndFixKinds ty')

-- | Check the kind structure of any types that show up in terms.
kcTerm :: KindAssumps -> Term -> TC Term
kcTerm env tm = case tm of
  Abs m       -> Abs <$> kcMatch env m
  Let ts us b -> Let <$> mapM (kcTypedDecl env) ts
                     <*> mapM (kcUntypedDecl env) us
                     <*> kcTerm env b
  App f xs    -> App <$> kcTerm env f <*> mapM (kcTerm env) xs
  Local{}     -> return tm
  Global{}    -> return tm
  Lit{}       -> return tm

-- | Check the kind structure of any types hiding under a binder.
kcMatch :: KindAssumps -> Match -> TC Match
kcMatch env m = case m of
  MTerm t   -> MTerm  <$> kcTerm env t
  MPat p m' -> MPat p <$> kcMatch env m'

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

inferKindTVar :: KindAssumps -> TVar -> TC (Kind,TVar)
inferKindTVar env tv = case tv of

  -- The idea here is that a variable might already have a kind associated with
  -- it through the environment, or that if it's a variable, that it might
  -- already have been unified with something concrete.
  UVar p -> do
    k' <- kindAssump (simpleName (paramName p)) env
    return (k', UVar p { paramKind = k' })

  GVar{} -> kindError "Unexpected generic variable"


-- Type Helpers ----------------------------------------------------------------

-- | Introduce new kind variables for each quantified type variable.  Give to a
-- continuation, an environment that contains those variables, and a type with
-- them instantiated.
introType :: KindAssumps -> Scheme -> (KindAssumps -> Type -> TC a) -> TC a
introType env (Forall ps ty) k = withVarIndex (length ps) $ do
  ps'  <- mapM freshTParam ps
  ty'  <- freshInst (Forall ps' ty)
  env' <- foldM (\e (q,t) -> addKindAssump e q t) env
      [ (simpleName (paramName p), paramKind p) | p <- ps' ]
  k env' ty'

-- | Given a type parameter, assign it a fresh kind variable.
freshTParam :: TParam -> TC TParam
freshTParam p = do
  k <- freshKindVar
  return p { paramKind = k }

-- | Quantify all variables in a type, fixing their kind variables to stars on
-- the way.
quantifyAndFixKinds :: Type -> Forall Type
quantifyAndFixKinds  = quantifyAll . mapTVar fixKind

-- | Turn kind variables into stars.
fixKind :: TVar -> TVar
fixKind tv = case tv of
  UVar p -> UVar (upd p)
  GVar p -> GVar (upd p)
  where
  upd p = p { paramKind = Types.apply s k }
    where
    k = paramKind p
    s = unboundSubst [ (paramIndex v,kstar) | v <- Set.toList (typeVars k) ]
