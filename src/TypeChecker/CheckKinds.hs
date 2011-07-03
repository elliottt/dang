{-# LANGUAGE DeriveDataTypeable #-}

module TypeChecker.CheckKinds where

import Dang.Monad
import Syntax.AST
import TypeChecker.Assume
import TypeChecker.Monad
import TypeChecker.Types
import Variables

import Control.Monad (foldM)
import Data.Typeable (Typeable)


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

  (pts',declEnv) <- kcPrimTerms env0 (modPrimTerms m)

  decls' <- mapM (kcDecl declEnv) (modDecls m)

  return m
    { modPrimTerms = pts'
    , modDecls     = decls'
    }

-- | Add a primitive type definition to the kind environment.  A good example of
-- one of these is the arrow constructor.
addPrimType :: PrimType -> KindEnv -> KindEnv
addPrimType pt = addAssump (Assume (primTypeName pt) (primTypeKind pt))

-- | Kind check individual primitive terms, fixing their kinds, and adding any
-- generated assumptions about the environment.
kcPrimTerms :: KindEnv -> [PrimTerm] -> TC ([PrimTerm],KindEnv)
kcPrimTerms env0 = foldM step ([],env0)
  where
  step (ps',env) pt = do
    (pt',env') <- kcPrimTerm env0 pt
    return (pt':ps', mergeAssumps env' env)

-- | Check the kind of a primitive term.  This should contain no variables, so
-- it's basically a sanity check.
kcPrimTerm :: KindEnv -> PrimTerm -> TC (PrimTerm,KindEnv)
kcPrimTerm env pt = do
  undefined

-- | Check the kind of the type of a declaration, then the kinds of any type
-- usages inside the term structure.
kcDecl :: KindEnv -> Decl -> TC Decl
kcDecl env d = do
  undefined

-- | Infer the kind of a type, fixing up internal kinds while we're at it.
inferKind :: KindEnv -> Type -> TC (Kind,Type)
inferKind env ty = case ty of

  TApp l r -> do
    (lk,l') <- inferKind env l
    (rk,r') <- inferKind env r
    a       <- freshKindVar
    unify (rk `karrow` a) lk
    return (a,TApp l' r')

  -- Infix constructors should have their kind in the environment already, so
  -- it's just a case of unifying the arguments and the kind of the constructor
  -- produce a result.
  TInfix n l r -> do
    nk      <- findKind n env
    (lk,l') <- inferKind env l
    (rk,r') <- inferKind env r
    unify nk (lk `karrow` rk `karrow` kstar)
    return (nk,TInfix n l' r')

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
  TGen _ p -> kindError "Unexpected generic variable"

  TNat _ -> return (knat,ty)
