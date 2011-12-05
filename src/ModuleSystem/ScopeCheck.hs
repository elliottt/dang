{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

module ModuleSystem.ScopeCheck where

import Dang.IO (logInfo,logDebug)
import Dang.Monad (Dang)
import ModuleSystem.Imports
import ModuleSystem.Interface
import ModuleSystem.Resolve
import ModuleSystem.Types
import Pretty (pretty)
import QualName
import Syntax.AST
    (Module(..),PrimTerm(..),UntypedDecl(..),TypedDecl(..),Match(..)
    ,DataDecl(..),Term(..))
import TypeChecker.Types (Type(..),Forall(..))
import qualified Data.ClashMap as CM

import Control.Applicative (Applicative,(<$>))
import Control.Monad ((<=<))
import MonadLib


-- Scope Checking Monad --------------------------------------------------------

data RO = RO
  { roNames :: ResolvedNames
  , roLevel :: !Int
  }

emptyRO :: RO
emptyRO  = RO
  { roNames = emptyResolvedNames
  , roLevel = 0
  }

newtype Scope a = Scope
  { unScope :: ReaderT RO Dang a
  } deriving (Functor,Applicative,Monad)

instance BaseM Scope Dang where
  inBase = Scope . inBase

runScope :: Scope a -> Dang a
runScope  = runReaderT emptyRO . unScope

withResolved :: ResolvedNames -> Scope a -> Scope a
withResolved names m = Scope $ do
  ro <- ask
  local (ro { roNames = names `mergeResolvedNames` roNames ro }) (unScope m)

resolve :: UsedName -> Scope Resolved
resolve u = Scope $ do
  let qn = usedQualName u
  ro <- ask
  case CM.clashElems <$> CM.lookup u (roNames ro) of
    Just [r] -> return r
    Just _   -> fail ("Symbol " ++ pretty qn ++ " is defined multiple times")
    _        -> fail ("Symbol " ++ pretty qn ++ " is not defined")

resolveType :: QualName -> Scope QualName
resolveType qn = do
  r <- resolve (UsedType qn)
  case r of
    Resolved _ qn -> return qn
    Bound _       -> fail "Unknown bound variable in type"

resolveTerm :: QualName -> Scope Term
resolveTerm qn = do
  r <- resolve (UsedTerm qn)
  case r of
    Resolved _ qn -> return (Global qn)
    Bound n       -> return (Local n)


-- Modules ---------------------------------------------------------------------

scopeCheckModule :: Module -> Scope (InterfaceSet,Module)
scopeCheckModule m = do
  logDebug "used symbols"
  let uses = getUses m
  logDebug (show uses)

  logInfo "minimal imports"
  let needed = minimalImports uses
  logInfo (pretty needed)

  logInfo "opening interfaces"
  iset <- loadInterfaces needed

  logDebug "resolved uses"
  let resolved = resolveUses iset uses
  logDebug (show resolved)

  withResolved resolved (scModule m)
  undefined

scModule :: Module -> Scope Module
scModule m = do
  ptms <- mapM scPrimTerm    (modPrimTerms m)
  us   <- mapM scUntypedDecl (modUntyped m)
  return m
    { modPrimTerms = ptms
    , modUntyped   = us
    }


-- Declarations ----------------------------------------------------------------

scPrimTerm :: PrimTerm -> Scope PrimTerm
scPrimTerm pt = do
  sc <- scForall scType (primTermType pt)
  return pt { primTermType = sc }

scUntypedDecl :: UntypedDecl -> Scope UntypedDecl
scUntypedDecl u = do
  tm <- scMatch (untypedBody u)
  return u { untypedBody = tm }

scTypedDecl :: TypedDecl -> Scope TypedDecl
scTypedDecl t = do
  sc <- scForall scType (typedType t)
  m  <- scMatch (typedBody t)
  return t { typedType = sc, typedBody = m }

scDataDecl :: DataDecl -> Scope DataDecl
scDataDecl dd = fail "scDataDecl"


-- Types -----------------------------------------------------------------------

scForall :: (a -> Scope a) -> (Forall a -> Scope (Forall a))
scForall k sc = do
  a <- k (forallData sc)
  return sc { forallData = a }

scType :: Type -> Scope Type
scType ty = case ty of
  TApp f x -> do
    f' <- scType f
    x' <- scType x
    return (TApp f' x')

  TInfix qn l r -> do
    qn' <- resolveType qn
    l'  <- scType l
    r'  <- scType r
    return (TInfix qn' l' r')

  TCon qn -> TCon <$> resolveType qn

  TVar _ -> return ty


-- Terms -----------------------------------------------------------------------

scMatch :: Match -> Scope Match
scMatch m = fail "scMatch"
