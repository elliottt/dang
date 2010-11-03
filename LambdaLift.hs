{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaLift (
    -- * Lambda Lifting Monad
    LL()
  , runLL
  , llDecls

    -- * Lambda Lifted AST
  , Decl(..)
  , Call(..)
  , Term(..)

    -- * Errors
  , LLError(..)
  ) where

import Error
import qualified AST

import Control.Applicative (Applicative(..),(<$>))
import Data.Graph (SCC(..))
import Data.List (partition,elemIndex)
import Data.Typeable (Typeable)
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set


type Subst = Map.Map AST.Var Term

newtype LL m a = LL
  { unLL :: StateT Subst (WriterT [Decl] m) a
  } deriving (Functor,Applicative,Monad)

runLL :: ExceptionM m i => LL m a -> m (a,[Decl])
runLL (LL m) = do
  ((a,_),ds) <- runWriterT (runStateT Map.empty m)
  return (a,ds)

instance BaseM m n => BaseM (LL m) n where
  inBase = LL . inBase

instance Monad m => WriterM (LL m) [Decl] where
  put = LL . put

instance Monad m => StateM (LL m) Subst where
  get = LL get
  set = LL . set

instance ExceptionM m SomeError => ExceptionM (LL m) SomeError where
  raise = LL . raise

instance RunExceptionM m SomeError => RunExceptionM (LL m) SomeError where
  try = LL . try . unLL

data LLError = LLError String
    deriving (Show, Typeable)

instance Error LLError

raiseLL :: ExceptionM m SomeError => String -> LL m a
raiseLL  = LL . raiseE . LLError

-- | Float a group of declarations out to the top-level, marking them as not
-- exported.
emits :: Monad m => [Decl] -> LL m ()
emits  = put . map notExported

extend :: Monad m => [(AST.Var,Term)] -> LL m ()
extend ns = do
  u <- get
  set $! Map.union (Map.fromList ns) u

subst :: ExceptionM m SomeError => [AST.Var] -> AST.Var -> LL m Term
subst args v = do
  u <- get
  case Map.lookup v u of
    Just t  -> return t
    Nothing ->
      case elemIndex v args of
        Just idx -> return (Argument idx)
        Nothing  -> raiseLL ("Unbound variable: " ++ v)

extendVars :: [AST.Var] -> Decl -> Decl
extendVars vs d = d { declVars = vs ++ declVars d }


-- AST -------------------------------------------------------------------------

data Decl = Decl
  { declName     :: String
  , declExported :: Bool
  , declVars     :: [String]
  , declBody     :: Term
  } deriving Show

notExported :: Decl -> Decl
notExported d = d { declExported = False }

hasArguments :: Decl -> Bool
hasArguments  = not . null . declVars

data Call
  = CFun String
  | CArg Int
    deriving Show

data Term
  = Apply Call [Term]
  | Let [Decl] Term
  | Symbol String
  | Argument Int
  | Lit AST.Literal
    deriving Show

apply :: Call -> [Term] -> Term
apply c xs
  | not (null xs) = Apply c xs
  | otherwise     =
    case c of
      CFun s -> Symbol s
      CArg i -> Argument i


-- Lambda Lifting --------------------------------------------------------------

-- | Lambda-lift a group of declarations, checking recursive declarations in a
-- group.
llDecls :: ExceptionM m SomeError => [AST.Decl] -> LL m [Decl]
llDecls ds = concat `fmap` mapM step (AST.sccDecls ds)
  where
  names               = Set.fromList (AST.declNames ds)
  step (AcyclicSCC d) = createClosure names [d]
  step (CyclicSCC ds) = createClosure names ds

-- | Rewrite references to declared names with applications that fill in its
-- free variables.  Augment the variables list for each declaration to include
-- the free variables. Descend, and lambda-lift each declaration individually.
createClosure :: ExceptionM m SomeError
              => Set.Set AST.Var -> [AST.Decl] -> LL m [Decl]
createClosure names ds = do
  let fvs = Set.toList (AST.freeVars ds Set.\\ names)
  rewriteFreeVars fvs ds
  mapM (fmap (extendVars fvs) . llDecl) ds

-- | Generate a mapping from the old symbol a declaration binds to an expression
-- that applies the free variables of that symbol.
rewriteFreeVars :: ExceptionM m SomeError
                => [AST.Var] -> [AST.Decl] -> LL m ()
rewriteFreeVars fvs ds =
  extend [ (n, apply (CFun n) syms) | d <- ds, let n = AST.declName d ]
  where
  syms = map Symbol fvs

-- | Lambda lift the body of a declaration.  Assume that all modifications to
-- the free variables have been performed already.
llDecl :: ExceptionM m SomeError => AST.Decl -> LL m Decl
llDecl d = do
  let args = AST.declVars d
  b' <- llTerm args (AST.declBody d)
  return Decl
    { declName     = AST.declName d
    , declExported = AST.declExported d
    , declVars     = args
    , declBody     = b'
    }

-- | Lambda lift terms.  Abstractions will cause an error here, as the invariant
-- for lambda-lifting is that they have been named in a let.
llTerm :: ExceptionM m SomeError => [AST.Var] -> AST.Term -> LL m Term
llTerm args t =
  case t of
    AST.Abs{}    -> raiseLL "llTerm: unexpected Abs"
    AST.App f xs -> do
      (c,as) <- llCall args f
      as'    <- mapM (llTerm args) as
      xs'    <- mapM (llTerm args) xs
      return (Apply c (as' ++ xs'))
    AST.Var v    -> subst args v
    AST.Lit l    -> return (Lit l)
    AST.Let ds e -> do
      ds' <- llDecls ds
      let (as,bs) = partition hasArguments ds'
      emits as
      e' <- llTerm args e
      if null bs
         then return e'
         else return (Let bs e')

llCall :: ExceptionM m SomeError
       => [AST.Var] -> AST.Term -> LL m (Call, [AST.Term])
llCall args t = do
  let (f,xs) = AST.splitApp t
  c <- llTerm args f
  case c of
    Symbol s   -> return (CFun s, xs)
    Argument i -> return (CArg i, xs)
    _          -> raiseLL ("llCall" ++ show f)
