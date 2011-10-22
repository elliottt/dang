{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Compile.LambdaLift (
    lambdaLift
  ) where

import Dang.IO
import Dang.Monad
import Core.AST
import Pretty (pretty)
import QualName (QualName,simpleName)
import TypeChecker.Types

import Control.Applicative (Applicative,(<$>))
import MonadLib
import qualified Data.Map as Map


lambdaLift :: Module -> Dang Module
lambdaLift m = do
  logStage "lambda-lifter"
  m' <- runLL (llModule m)
  logInfo "lambda-lifter output"
  logDebug (show m')
  logInfo (pretty m')
  return m'


-- Lambda-lifting Monad --------------------------------------------------------

newtype LL a = LL
  { unLL :: StateT Subst (ReaderT Env (WriterT [Decl] Dang)) a
  } deriving (Functor,Applicative,Monad)

instance BaseM LL Dang where
  inBase = LL . inBase

instance WriterM LL [Decl] where
  put = LL . put

instance RunWriterM LL [Decl] where
  collect m = LL (collect (unLL m))

instance ReaderM LL Env where
  ask = LL ask

instance RunReaderM LL Env where
  local env m = LL (local env (unLL m))

runLL :: LL a -> Dang a
runLL (LL m) =
  f `fmap` runWriterT (runReaderT emptyEnv (runStateT emptySubst m))
  where
  f = fst . fst


-- Substitution ----------------------------------------------------------------

type Subst = Map.Map QualName Term

emptySubst :: Subst
emptySubst  = Map.empty

-- | Produce either a reference to a global variable, or the term that it is to
-- be replaced with.
subst :: QualName -> LL Term
subst qn = LL $ do
  u <- get
  case Map.lookup qn u of
    Just tm -> return tm
    Nothing -> return (Global qn)

rename :: QualName -> Term -> LL ()
rename qn tm = LL $ do
  u <- get
  set $! Map.insert qn tm u


-- Type Environment ------------------------------------------------------------

data Name
  = NLocal  Var
  | NGlobal QualName
    deriving (Eq,Show,Ord)

type Env = Map.Map Name Type

emptyEnv :: Env
emptyEnv  = Map.empty

addLocal :: Var -> Type -> Env -> Env
addLocal  = Map.insert . NLocal

addGlobal :: QualName -> Type -> Env -> Env
addGlobal  = Map.insert . NGlobal

introPat :: Pat -> LL a -> LL a
introPat p k = case p of

  PVar v ty -> do
    env <- ask
    local (addLocal v ty env) k

  PWildcard _ -> k


-- Core Lambda-lifting ---------------------------------------------------------

llModule :: Module -> LL Module
llModule m = do
  (ds',ls) <- collect (mapM llTopDecl (modDecls m))
  return m { modDecls = ds' ++ ls }

llTopDecl :: Decl -> LL Decl
llTopDecl d = do
  logInfo ("processing " ++ pretty (declName d))
  b' <- llForall llMatch (declBody d)
  return d { declBody = b' }

llForall :: (a -> LL a) -> Forall a -> LL (Forall a)
llForall k (Forall ps a) = Forall ps <$> k a

llMatch :: Match -> LL Match
llMatch m = case m of

  MPat p b -> MPat p <$> introPat p (llMatch b)

  MTerm tm ty -> do
    tm' <- llTerm tm
    return (MTerm tm' ty)

-- | Process a series of declarations from a let-expression, lifting out any
-- that contain arguments, and generating appropriate substitutions.
llLetDecls :: [Decl] -> LL [Decl]
llLetDecls ds = fail "llLetDecls"

llTerm :: Term -> LL Term
llTerm tm = case tm of

  AppT f ts -> do
    f' <- llTerm f
    return (appT f' ts)

  App f xs -> do
    f'  <- llTerm f
    xs' <- mapM llTerm xs
    return (app f' xs')

  Let ds e -> do
    ds' <- llLetDecls ds
    e'  <- llTerm e
    return (Let ds' e')

  Global qn -> subst qn

  Local _ -> return tm
  Lit _   -> return tm
