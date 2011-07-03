{-# LANGUAGE DeriveDataTypeable #-}

module TypeChecker.CheckKinds where

import Dang.Monad
import Syntax.AST
import TypeChecker.Assume
import TypeChecker.Monad
import TypeChecker.Types
import TypeChecker.Unify as Types

import Control.Applicative ((<$>),(<*>))
import Data.Typeable (Typeable)
import qualified Data.Traversable as T
import qualified Data.Set as Set


-- Utilities -------------------------------------------------------------------

freshKindVar :: TC Kind
freshKindVar  = freshVar setSort

type KindAssump = Assump Kind

type KindEnv = Assumps Sort

findKind :: String -> KindEnv -> TC Kind
findKind  = findAssump

data KindError = KindError String
    deriving (Typeable,Show)

instance Exception KindError

kindError :: String -> TC a
kindError  = raiseE . KindError


-- Kind Checking ---------------------------------------------------------------

-- | Check the kinds of all type usages in a Module.  Return a Module that
-- contains all it's declarations with fixed kinds.
kcModule :: KindEnv -> Module -> TC Module
kcModule env m = do
  let env0 = foldr addPrimType env (modPrimTypes m)

  pts'   <- mapM (kcPrimTerm env0) (modPrimTerms m)
  decls' <- mapM (kcDecl env0) (modDecls m)

  return m
    { modPrimTerms = pts'
    , modDecls     = decls'
    }

-- | Add a primitive type definition to the kind environment.  A good example of
-- one of these is the arrow constructor.
addPrimType :: PrimType -> KindEnv -> KindEnv
addPrimType pt = addAssump (Assume (primTypeName pt) (primTypeKind pt))

-- | Check the kind of a primitive term.  This should contain no variables, so
-- it's basically a sanity check.
kcPrimTerm :: KindEnv -> PrimTerm -> TC PrimTerm
kcPrimTerm env pt = do
  qt <- kcTypeSig env (primTermType pt)
  return pt { primTermType = qt }

-- | Check the kind of the type of a declaration, then the kinds of any type
-- usages inside the term structure.
kcDecl :: KindEnv -> Decl -> TC Decl
kcDecl env d = do
  qt <- T.sequenceA (kcTypeSig env `fmap` declType d)
  b  <- kcTerm env (declBody d)
  return d { declType = qt, declBody = b }

-- | Introduce kind variables for all type variables, and kind check a type
-- signature.
kcTypeSig :: KindEnv -> Forall Type -> TC (Forall Type)
kcTypeSig env0 qt = introType qt $ \ vars ty -> do
  let env = mergeAssumps vars env0
  (tyk,ty') <- inferKind env ty
  unify kstar tyk
  return (quantifyAll ty')

-- | Check the kind structure of any types that show up in terms.
kcTerm :: KindEnv -> Term -> TC Term
kcTerm env tm = case tm of
  Abs vs b -> Abs vs <$> kcTerm env b
  Let ds b -> Let    <$> mapM (kcDecl env) ds <*> kcTerm env b
  App f xs -> App    <$> kcTerm env f         <*> mapM (kcTerm env) xs
  Local{}  -> return tm
  Global{} -> return tm
  Lit{}    -> return tm
  Prim{}   -> return tm

-- | Infer the kind of a type, fixing up internal kinds while we're at it.
inferKind :: KindEnv -> Type -> TC (Kind,Type)
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
    nk      <- findKind n env
    res     <- freshKindVar
    unify nk (lk `karrow` rk `karrow` res)
    res'    <- applySubst res
    return (res',TInfix n l' r')

  -- Constructors should just have their kinds already specified in the
  -- environment.  There's no need to check them, as they should only be
  -- constructed by the compiler.
  TCon n -> do
    k <- findKind n env
    return (k,ty)

  -- The idea here is that a variable might already have a kind associated with
  -- it through the environment, or that if it's a variable, that it might
  -- already have been unified with something concrete.
  TVar i p -> do
    k' <- applySubst =<< findKind (paramName p) env
    return (k', TVar i p { paramKind = k' })

  -- All generic variables should disappear through instantiation, so this
  -- shouldn't happen.
  TGen{} -> kindError "Unexpected generic variable"

  TNat{} -> return (knat,ty)


-- Type Helpers ----------------------------------------------------------------

-- | Introduce new kind variables for each quantified type variable.  Give to a
-- continuation, an environment that contains those variables, and a type with
-- them instantiated.
introType :: Forall Type -> (KindEnv -> Type -> TC a) -> TC a
introType (Forall ps ty) k = withVarIndex (length ps) $ do
  ps' <- mapM freshTParam ps
  let env = [ Assume (paramName p) (paramKind p) | p <- ps' ]
  ty' <- inst (zipWith TVar [0..] ps') ty
  k env ty'

-- | Given a type parameter, assign it a fresh kind variable.
freshTParam :: TParam -> TC TParam
freshTParam p = do
  k <- freshKindVar
  return p { paramKind = k }

-- | Quantify all variables in a type, fixing their kind variables to stars on
-- the way.
quantifyAll :: Type -> Forall Type
quantifyAll ty = Forall ps (Types.apply s ty)
  where
  vs          = Set.toList (typeVars ty)
  ps          = map fixKind vs
  step v n p' = (v, TGen n p')
  s           = Subst (zipWith3 step vs [0 ..] ps)

-- | Turn kind variables into stars.
fixKind :: TParam -> TParam
fixKind p = p { paramKind = Types.apply s k }
  where
  k = paramKind p
  s = Subst [ (v,kstar) | v <- Set.toList (typeVars k) ]
