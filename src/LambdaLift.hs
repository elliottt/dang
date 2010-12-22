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
  , llModule

    -- * Lambda Lifted AST
  , Decl(..)
  , LetDecl(..)
  , Term(..)

    -- * Errors
  , LLError(..)
  ) where

import Dang.Monad
import Pretty
import Prim
import QualName
import qualified Syntax.AST as AST

import Control.Applicative (Applicative(..))
import Data.Graph (SCC(..))
import Data.Int (Int32)
import Data.List (partition,elemIndex)
import Data.Typeable (Typeable)
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set


type Subst = Map.Map QualName Term

data RO = RO
  { roVars    :: Set.Set QualName
  , roContext :: [Name]
  , roSubst   :: Subst
  }

emptyRO :: RO
emptyRO  = RO
  { roVars    = Set.empty
  , roContext = []
  , roSubst   = Map.empty
  }

-- | The Lambda-lifting monad transformer.
-- It is a reader for bound variables, state for a name substitution, and a
-- writer for lifted declarations.
newtype LL a = LL
  { unLL :: ReaderT RO (WriterT [Decl] Dang) a
  } deriving (Functor,Applicative,Monad)

runLL :: LL a -> Dang (a,[Decl])
runLL (LL m) = do
  (a,ds) <- runWriterT (runReaderT emptyRO m)
  return (a,ds)

instance BaseM LL IO where
  inBase = LL . inBase

instance ReaderM LL RO where
  ask = LL ask

instance RunReaderM LL RO where
  local ro = LL . local ro . unLL

instance WriterM LL [Decl] where
  put = LL . put

instance ExceptionM LL SomeException where
  raise = LL . raise

instance RunExceptionM LL SomeException where
  try = LL . try . unLL

data LLError = LLError String
    deriving (Show, Typeable)

instance Exception LLError

raiseLL :: String -> LL a
raiseLL  = LL . raiseE . LLError

-- | Float a group of declarations out to the top-level, marking them as not
-- exported.
emits :: [Decl] -> LL ()
emits  = put . map notExported

extend :: [(QualName,Term)] -> LL a -> LL a
extend ns m = do
  ro <- ask
  local (ro { roSubst = Map.union (Map.fromList ns) (roSubst ro) }) m

subst :: [AST.Var] -> QualName -> LL Term
subst args qn = do
  ro <- ask
  case Map.lookup qn (roSubst ro) of
    Just t  -> return t
    Nothing ->
      case guard (isSimpleName qn) >> elemIndex (qualSymbol qn) args of
        Just idx -> return (Argument (fromIntegral idx))
        Nothing  -> raiseLL ("Unbound variable: " ++ pretty qn)

extendVars :: [AST.Var] -> AST.Decl -> AST.Decl
extendVars vs d = d { AST.declVars = vs ++ AST.declVars d }

bindVars :: Set.Set QualName -> LL a -> LL a
bindVars vs m = do
  ro <- ask
  let ro' = ro { roVars = Set.union vs (roVars ro) }
  local ro' m

namespace :: [Name] -> LL a -> LL a
namespace ns m = do
  ro <- ask
  let ro' = ro { roContext = ns }
  local ro' m

prefix :: LL [Name]
prefix  = roContext `fmap` ask


-- AST -------------------------------------------------------------------------

-- | Lambda-lifted declarations.  The variables only serve as documentation at
-- this point, both to describe the arity of the closure, and the names of the
-- variables used in Argument term nodes.
data Decl = Decl
  { declName     :: QualName
  , declExported :: Bool
  , declVars     :: [String]
  , declBody     :: Term
  } deriving Show

instance Pretty Decl where
  pp _ d = pp 0 (declName d) <+> pp 0 (declVars d) <+> char '='
       <+> pp 0 (declBody d)
  ppList _ ds = semis (map (pp 0) ds)

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

data Term
  = Apply Term [Term]
  | Let [LetDecl] Term
  | Symbol QualName
  | Var QualName
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
  pp _ (Symbol qn)   = pp 0 qn
  pp _ (Var qn)      = pp 0 qn
  pp _ (Argument i)  = char '$' <> ppr i
  pp _ (Lit l)       = pp 0 l
  pp p (Prim n a as) = optParens (p > 0)
                     $ text n <> brackets (int a) <+> ppList 1 as


apply :: Term -> [Term] -> Term
apply f [] = f
apply f xs = Apply f xs


-- Lambda Lifting --------------------------------------------------------------

-- | Lambda-lift a module into a collection of declarations.
llModule :: AST.Module -> LL [Decl]
llModule m = namespace (AST.modNamespace m)
           $ introSymbols True ds
           $ llDecls True ds
  where
  ds = AST.modDecls m

introSymbols :: Bool -> [AST.Decl] -> LL a -> LL a
introSymbols ex ds m = do
  ro <- ask
  let name d | ex        = qualName (roContext ro) (AST.declName d)
             | otherwise = simpleName (AST.declName d)
      qs' = Map.fromList [ (n,Symbol n) | d <- ds, let n = name d ]
  local (ro { roSubst = Map.union qs' (roSubst ro) }) m

-- | Lambda-lift a group of declarations, checking recursive declarations in a
-- group.
llDecls :: Bool -> [AST.Decl] -> LL [Decl]
llDecls ex ds = do
  ns <- prefix
  let names               = Set.fromList (map (qualName ns) (AST.declNames ds))
      step (AcyclicSCC d) = bindVars names (createClosure ex [d])
      step (CyclicSCC rs) = bindVars names (createClosure ex rs)
  concat `fmap` mapM step (AST.sccDecls ns ds)

-- | Rewrite references to declared names with applications that fill in its
-- free variables.  Augment the variables list for each declaration to include
-- the free variables. Descend, and lambda-lift each declaration individually.
-- It's OK to extend the arguments here, as top-level functions won't have free
-- variables.
createClosure :: Bool -> [AST.Decl] -> LL [Decl]
createClosure ex ds = do
  ro <- LL ask
  let names = roVars ro
  let fvs   = AST.freeVars ds Set.\\ names
  let fvl   = map qualSymbol (Set.toList fvs)
  rewriteFreeVars ex fvl ds
    $ bindVars fvs (mapM (llDecl ex . extendVars fvl) ds)

-- | Generate a mapping from the old symbol a declaration binds to an expression
-- that applies the free variables of that symbol.  One key assumption made here
-- is that free variables are always coming from the scope of the enclosing
-- function.
rewriteFreeVars :: Bool -> [AST.Var] -> [AST.Decl] -> LL a -> LL a
rewriteFreeVars ex fvs ds m = do
  ps <- prefix
  let name d | ex        = qualName ps (AST.declName d)
             | otherwise = simpleName (AST.declName d)
  extend [ (name d, apply (Var n) args)
         | d <- ds, let n = name d ] m
  where
  args = zipWith (const . Argument) [0 ..] fvs

-- | Lambda lift the body of a declaration.  Assume that all modifications to
-- the free variables have been performed already.
llDecl :: Bool -> AST.Decl -> LL Decl
llDecl ex d = do
  let args = AST.declVars d
  b' <- llTerm args (AST.declBody d)
  ps <- prefix
  let name | ex        = qualName ps (AST.declName d)
           | otherwise = simpleName (AST.declName d)
  return Decl
    { declName     = name
    , declExported = ex
    , declVars     = args
    , declBody     = b'
    }

-- | Translate from a top-level declaration to a let declaration, which
-- shouldn't introduce new variables to its body.
llLetDecl :: Decl -> LetDecl
llLetDecl d = LetDecl
  { letName = sym
  , letBody = declBody d
  }
  where
  n   = declName d
  sym = qualSymbol n

llLetDecls :: [AST.Decl] -> ([LetDecl] -> LL a) -> LL a
llLetDecls ds k = do
  ds' <- llDecls False ds
  let (as,bs) = partition hasArguments ds'
  emits as
  let ls = map llLetDecl bs
  introSymbols False ds (k ls)

-- | Lambda lift terms.  Abstractions will cause an error here, as the invariant
-- for lambda-lifting is that they have been named.
llTerm :: [AST.Var] -> AST.Term -> LL Term
llTerm args t =
  case t of
    AST.Abs{}    -> raiseLL "llTerm: unexpected Abs"
    AST.Prim n   -> llPrim args n []
    AST.App f xs -> llApp args f xs
    AST.Var qn   -> subst args qn
    AST.Lit l    -> return (Lit l)
    AST.Let ds e -> llLetDecls ds $ \ls -> do
      e' <- llTerm args e
      if null ls
         then return e'
         else return (Let ls e')

llApp :: [AST.Var] -> AST.Term -> [AST.Term] -> LL Term
llApp args t xs =
  case t of
    AST.Prim n -> llPrim args n xs
    _          -> do
      t'  <- llTerm args t
      xs' <- mapM (llTerm args) xs
      return (Apply t' xs')

llPrim :: [AST.Var] -> String -> [AST.Term] -> LL Term
llPrim args n xs = do
  arity <- primArity n
  unless (arity == length xs)
    (raiseLL ("Not enough arguments for primitive: " ++ n))
  Prim n arity `fmap` mapM (llTerm args) xs

primArity :: String -> LL Int
primArity n =
  case lookup n primitives of
    Nothing -> raiseLL ("Unknown primitive: " ++ n)
    Just i  -> return i
