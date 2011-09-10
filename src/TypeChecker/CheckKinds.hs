{-# LANGUAGE DeriveDataTypeable #-}

module TypeChecker.CheckKinds where

import Dang.IO
import Dang.Monad
import Pretty
import QualName
import Syntax.AST
import TypeChecker.Monad
import TypeChecker.Types
import TypeChecker.Unify as Types

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (second)
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


-- Kind Checking ---------------------------------------------------------------

-- | Check the kinds of all type usages in a Module.  Return a Module that
-- contains all it's declarations with fixed kinds.
kcModule :: Module -> TC Module
kcModule m = addKindBindings (map primTypeBinding (modPrimTypes m)) $ do

  pts' <- mapM kcPrimTerm (modPrimTerms m)
  ts'  <- mapM kcTypedDecl (modTyped m)
  us'  <- mapM kcUntypedDecl (modUntyped m)

  return m
    { modPrimTerms = pts'
    , modTyped     = ts'
    , modUntyped   = us'
    }

primTypeBinding :: PrimType -> (QualName,Kind)
primTypeBinding pt = (primName (primTypeName pt), primTypeKind pt)

-- | Check the kind of a primitive term.  This should contain no variables, so
-- it's basically a sanity check.
kcPrimTerm :: PrimTerm -> TC PrimTerm
kcPrimTerm pt = do
  qt <- kcTypeSig (primTermType pt)
  return pt { primTermType = qt }

-- | Check the kind of the type of a declaration, then the kinds of any type
-- usages inside the term structure.
kcTypedDecl :: TypedDecl -> TC TypedDecl
kcTypedDecl d = do
  qt <- kcTypeSig (typedType d)
  b  <- kcMatch (typedBody d)
  return d
    { typedType = qt
    , typedBody = b
    }

-- | Check the kinds of any types used within the body of an untyped
-- declaration.
kcUntypedDecl :: UntypedDecl -> TC UntypedDecl
kcUntypedDecl d = do
  b <- kcMatch (untypedBody d)
  return d { untypedBody = b }

-- | Introduce kind variables for all type variables, and kind check a type
-- signature.
kcTypeSig :: Forall Type -> TC (Forall Type)
kcTypeSig qt = introType qt $ \ ty -> do
  logInfo ("Introduced Type: " ++ pretty ty)
  logDebug (show (typeVars ty))
  (tyk,ty') <- inferKind ty
  unify kstar tyk
  return (quantifyAll ty')

-- | Check the kind structure of any types that show up in terms.
kcTerm :: Term -> TC Term
kcTerm tm = case tm of
  Abs m       -> Abs <$> kcMatch m
  Let ts us b -> Let <$> mapM kcTypedDecl ts <*> mapM kcUntypedDecl us
                     <*> kcTerm b
  App f xs    -> App <$> kcTerm f <*> mapM kcTerm xs
  Local{}     -> return tm
  Global{}    -> return tm
  Lit{}       -> return tm
  Prim{}      -> return tm

-- | Check the kind structure of any types hiding under a binder.
kcMatch :: Match -> TC Match
kcMatch m = case m of
  MTerm t   -> MTerm  <$> kcTerm t
  MPat p m' -> MPat p <$> kcMatch m'

-- | Infer the kind of a type, fixing up internal kinds while we're at it.
inferKind :: Type -> TC (Kind,Type)
inferKind ty = case ty of

  TApp l r -> do
    (lk,l') <- inferKind l
    (rk,r') <- inferKind r
    a       <- freshKindVar
    unify (rk `karrow` a) lk
    a'      <- applySubst a
    return (a',TApp l' r')

  -- Infix constructors should have their kind in the environment already, so
  -- it's just a case of unifying the arguments and the kind of the constructor
  -- produce a result.
  TInfix n l r -> do
    (lk,l') <- inferKind l
    (rk,r') <- inferKind r
    nk      <- findKind n
    res     <- freshKindVar
    unify nk (lk `karrow` rk `karrow` res)
    res'    <- applySubst res
    return (res',TInfix n l' r')

  -- Constructors should just have their kinds already specified in the
  -- environment.  There's no need to check them, as they should only be
  -- constructed by the compiler.
  TCon n -> do
    k <- findKind n
    return (k,ty)

  -- The idea here is that a variable might already have a kind associated with
  -- it through the environment, or that if it's a variable, that it might
  -- already have been unified with something concrete.
  TVar i p -> do
    k' <- findKind (simpleName (paramName p))
    return (k', TVar i p { paramKind = k' })

  -- All generic variables should disappear through instantiation, so this
  -- shouldn't happen.
  TGen{} -> kindError "Unexpected generic variable"


-- Type Helpers ----------------------------------------------------------------

-- | Introduce new kind variables for each quantified type variable.  Give to a
-- continuation, an environment that contains those variables, and a type with
-- them instantiated.
introType :: Forall Type -> (Type -> TC a) -> TC a
introType (Forall ps ty) k = withVarIndex (length ps) $ do
  ps' <- mapM freshTParam ps
  ty' <- inst (zipWith TVar [0..] ps') ty
  let binds = [ (simpleName (paramName p), paramKind p) | p <- ps' ]
  addKindBindings binds (k ty')

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
  (is,ps)     = unzip (Set.toList (Set.map (second fixKind) (typeVars ty)))
  step i n p' = (i, TGen n p')
  s           = Subst (zipWith3 step is [0 ..] ps)

-- | Turn kind variables into stars.
fixKind :: TParam -> TParam
fixKind p = p { paramKind = Types.apply s k }
  where
  k = paramKind p
  s = Subst [ (i,kstar) | (i,_) <- Set.toList (typeVars k) ]
