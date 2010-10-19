{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LambdaLift (
    -- * Lambda Lifting Monad
    LL()
  , runLL
  , llDecls

    -- * Errors
  , LLError(..)
  ) where

import AST
import Error

import Control.Applicative (Applicative(..),(<$>))
import Data.Graph (SCC(..))
import Data.List (partition)
import Data.Typeable (Typeable)
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set


type Subst = Map.Map Var Term

newtype LL m a = LL (StateT Subst (WriterT [Decl] m) a)
    deriving (Functor,Applicative,Monad)

runLL :: ExceptionM m i => LL m a -> m (a,[Decl])
runLL (LL m) = do
  ((a,_),ds) <- runWriterT (runStateT Map.empty m)
  return (a,ds)

instance Monad m => WriterM (LL m) [Decl] where
  put = LL . put

instance Monad m => StateM (LL m) Subst where
  get = LL get
  set = LL . set

data LLError = LLError String
    deriving Typeable

instance Error LLError

raiseLL :: ExceptionM m SomeError => String -> LL m a
raiseLL  = LL . raiseError . LLError

-- | Float a group of declarations out to the top-level, marking them as not
-- exported.
emits :: Monad m => [Decl] -> LL m ()
emits  = put . map notExported

extend :: Monad m => [(Var,Term)] -> LL m ()
extend ns = do
  u <- get
  set $! Map.union (Map.fromList ns) u

subst :: Monad m => Var -> LL m Term
subst v = do
  u <- get
  case Map.lookup v u of
    Nothing -> return (Var v)
    Just t  -> return t

extendVars :: [Var] -> Decl -> Decl
extendVars vs d = d { declVars = vs ++ declVars d }

-- | Lambda-lift a group of declarations, checking recursive declarations in a
-- group.
llDecls :: ExceptionM m SomeError => [Decl] -> LL m [Decl]
llDecls  = fmap concat . mapM step . sccDecls
  where
  step (AcyclicSCC d) = createClosure [d]
  step (CyclicSCC ds) = createClosure ds

-- | Rewrite references to declared names with applications that fill in its
-- free variables.  Augment the variables list for each declaration to include
-- the free variables. Descend, and lambda-lift each declaration individually.
createClosure :: ExceptionM m SomeError => [Decl] -> LL m [Decl]
createClosure ds = do
  let ns  = declNames ds
  let fvs = Set.toList (freeVars ds Set.\\ Set.fromList ns)
  extend [ (n, apply (Var n) (map Var fvs)) | n <- ns ]
  mapM (fmap (extendVars fvs) . llDecl) ds

-- | Lambda lift the body of a declaration.  Assume that all modifications to
-- the free variables have been performed already.
llDecl :: ExceptionM m SomeError => Decl -> LL m Decl
llDecl d = do
  b' <- llTerm (declBody d)
  return d { declBody = b' }

-- | Lambda lift terms.  Abstractions will cause an error here, as the invariant
-- for lambda-lifting is that they have been named in a let.
llTerm :: ExceptionM m SomeError => Term -> LL m Term
llTerm t =
  case t of
    Abs{}    -> raiseLL "Abs should have been removed during renaming"
    App f x  -> App <$> llTerm f <*> llTerm x
    Var v    -> subst v
    Lit l    -> return (Lit l)
    Let ds e -> do
      ds' <- llDecls ds
      let (as,bs) = partition hasArguments ds'
      emits as
      e' <- llTerm e
      if null bs
         then return e'
         else return (Let bs e')
