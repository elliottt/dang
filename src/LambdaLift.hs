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
  , LetDecl(..)
  , Call(..)
  , Term(..)

    -- * Errors
  , LLError(..)
  ) where

import Error
import Pretty
import Prim
import qualified AST

import Control.Applicative (Applicative(..))
import Data.Graph (SCC(..))
import Data.Int (Int32)
import Data.List (partition,elemIndex)
import Data.Typeable (Typeable)
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set


type Subst = Map.Map AST.Var Term

-- | The Lambda-lifting monad transformer.
-- It is a reader for bound variables, state for a name substitution, and a
-- writer for lifted declarations.
newtype LL m a = LL
  { unLL :: ReaderT (Set.Set AST.Var) (StateT Subst (WriterT [Decl] m)) a
  } deriving (Functor,Applicative,Monad)

runLL :: ExceptionM m i => LL m a -> m (a,[Decl])
runLL (LL m) = do
  ((a,_),ds) <- runWriterT (runStateT Map.empty (runReaderT Set.empty m))
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
        Just idx -> return (Argument (fromIntegral idx))
        Nothing  -> raiseLL ("Unbound variable: " ++ v)

extendVars :: [AST.Var] -> AST.Decl -> AST.Decl
extendVars vs d = d { AST.declVars = vs ++ AST.declVars d }

bindVars :: ExceptionM m SomeError => Set.Set AST.Var -> LL m a -> LL m a
bindVars vs (LL m) = LL $ do
  vs0 <- ask
  local (Set.union vs vs0) m


-- AST -------------------------------------------------------------------------

-- | Lambda-lifted declarations.  The variables only serve as documentation at
-- this point, both to describe the arity of the closure, and the names of the
-- variables used in Argument term nodes.
data Decl = Decl
  { declName     :: String
  , declExported :: Bool
  , declVars     :: [String]
  , declBody     :: Term
  } deriving Show

instance Pretty Decl where
  pp _ d =  text (declName d) <+> pp 0 (declVars d) <+> char '='
        <+> pp 0 (declBody d)

notExported :: Decl -> Decl
notExported d = d { declExported = False }

hasArguments :: Decl -> Bool
hasArguments  = not . null . declVars

data LetDecl = LetDecl
  { letName :: String
  , letBody :: Term
  } deriving Show

instance Pretty LetDecl where
  pp     _ d  = text (letName d) <+> char '=' <+> pp 0 (letBody d)
  ppList _ ds = braces (semis (map (pp 0) ds))

data Call
  = CFun  String
  | CArg  Int32
    deriving Show

instance Pretty Call where
  pp p (CFun s)  = optParens (p > 0) (text "alloc_closure" <+> text s)
  pp _ (CArg i)  = char '$' <> ppr i

data Term
  = Apply Call [Term]
  | Let [LetDecl] Term
  | Symbol String
  | Var String
  | Argument Int32
  | Lit AST.Literal
  | Prim String Int [Term]
    deriving Show

instance Pretty Term where
  pp p (Apply c ts)  = optParens (p > 0)
                     $ text "apply" <+> pp 1 c <+> ppList 1 ts
  pp p (Let ds t)    = optParens (p > 0)
                     $ text "let" <+> braces (ppList 0 ds) <+> text "in"
                   <+> pp 0 t
  pp p (Symbol s)    = optParens (p > 0) (text "alloc_closure" <+> text s)
  pp _ (Var n)       = text n
  pp _ (Argument i)  = char '$' <> ppr i
  pp _ (Lit l)       = pp 0 l
  pp p (Prim n a as) = optParens (p > 0)
                     $ text n <> brackets (int a) <+> ppList 1 as


apply :: Call -> [Term] -> Term
apply (CFun s)  [] = Symbol s
apply (CArg i)  [] = Argument i
apply c         xs = Apply c xs


-- Lambda Lifting --------------------------------------------------------------

-- | Lambda-lift a group of declarations, checking recursive declarations in a
-- group.
llDecls :: ExceptionM m SomeError => [AST.Decl] -> LL m [Decl]
llDecls ds = concat `fmap` mapM step (AST.sccDecls ds)
  where
  names               = Set.fromList (AST.declNames ds)
  step (AcyclicSCC d) = bindVars names (createClosure [d])
  step (CyclicSCC rs) = bindVars names (createClosure rs)

-- | Rewrite references to declared names with applications that fill in its
-- free variables.  Augment the variables list for each declaration to include
-- the free variables. Descend, and lambda-lift each declaration individually.
-- It's OK to extend the arguments here, as top-level functions won't have free
-- variables.
createClosure :: ExceptionM m SomeError
              => [AST.Decl] -> LL m [Decl]
createClosure ds = do
  names <- LL ask
  let fvs = AST.freeVars ds Set.\\ names
  let fvl = Set.toList fvs
  rewriteFreeVars fvl ds
  bindVars fvs (mapM (llDecl . extendVars fvl) ds)

-- | Generate a mapping from the old symbol a declaration binds to an expression
-- that applies the free variables of that symbol.  One key assumption made here
-- is that free variables are always coming from the scope of the enclosing
-- function.
rewriteFreeVars :: ExceptionM m SomeError
                => [AST.Var] -> [AST.Decl] -> LL m ()
rewriteFreeVars fvs ds =
  extend [ (n, apply (CFun n) args) | d <- ds, let n = AST.declName d ]
  where
  args = zipWith (const . Argument) [0 ..] fvs

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

-- | Translate from a top-level declaration to a let declaration, which
-- shouldn't introduce new variables to its body.
llLetDecl :: ExceptionM m SomeError => Decl -> LL m LetDecl
llLetDecl d = do
  unless (null (declVars d)) (raiseLL "Function remained in let-decls")
  let n = declName d
  extend [(n,Var n)]
  return LetDecl
    { letName = n
    , letBody = declBody d
    }

-- | Lambda lift terms.  Abstractions will cause an error here, as the invariant
-- for lambda-lifting is that they have been named.
llTerm :: ExceptionM m SomeError => [AST.Var] -> AST.Term -> LL m Term
llTerm args t =
  case t of
    AST.Abs{}    -> raiseLL "llTerm: unexpected Abs"
    AST.Prim n   -> llPrim args n []
    AST.App f xs -> llApp args f xs
    AST.Var v    -> subst args v
    AST.Lit l    -> return (Lit l)
    AST.Let ds e -> do
      ds' <- llDecls ds
      let (as,bs) = partition hasArguments ds'
      emits as
      ls <- mapM llLetDecl bs
      e' <- llTerm args e
      if null bs
         then return e'
         else return (Let ls e')

llApp :: ExceptionM m SomeError
      => [AST.Var] -> AST.Term -> [AST.Term] -> LL m Term
llApp args t xs = do
  case AST.splitApp t of
    (AST.Prim n, ys) -> llPrim args n (ys ++ xs)
    (f, ys)      -> do
      c  <- llTerm args f
      ts <- mapM (llTerm args) (ys ++ xs)
      case c of
        Symbol s   -> return (Apply (CFun s) ts)
        Argument i -> return (Apply (CArg i) ts)
        Apply g as -> return (Apply g (as ++ ts))
        _          -> raiseLL ("llCall: " ++ show c)

llPrim :: ExceptionM m SomeError
       => [AST.Var] -> String -> [AST.Term] -> LL m Term
llPrim args n xs = do
  arity <- primArity n
  unless (arity == length xs)
    (raiseLL ("Not enough arguments for primitive: " ++ n))
  Prim n arity `fmap` mapM (llTerm args) xs

primArity :: ExceptionM m SomeError
          => String -> LL m Int
primArity n =
  case lookup n primitives of
    Nothing -> raiseLL ("Unknown primitive: " ++ n)
    Just i  -> return i
