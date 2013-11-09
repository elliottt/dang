{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Trustworthy #-}

module Dang.ModuleSystem.ScopeCheck where

import Dang.IO (logInfo,logDebug)
import Dang.ModuleSystem.Imports
import Dang.ModuleSystem.Interface
import Dang.ModuleSystem.Resolve
import Dang.ModuleSystem.Types
import Dang.Monad (Dang)
import Dang.Syntax.AST
    (Module(..),UntypedDecl(..),TypedDecl(..),Match(..),Term(..),Pat(..)
    ,DataDecl(..),patVars)
import Dang.ModuleSystem.QualName
import Dang.TypeChecker.Types (Type(..),Kind)
import Dang.TypeChecker.Vars (TParam())
import Dang.Utils.Pretty (pretty)
import qualified Data.ClashMap as CM

import Control.Applicative (Applicative,(<$>))
import Control.Monad ( MonadPlus )
import Data.Generics (Data(gmapM),extM)
import MonadLib ( BaseM(..), ReaderT, ask, local, runM )
import qualified Data.Set as Set


-- Scope Checking Monad --------------------------------------------------------

data RO = RO
  { roNames :: ResolvedNames
    -- ^ A map of parsed names to resolved names.
  , roLevel :: !Int
    -- ^ Lexical scope levels
  }

emptyRO :: RO
emptyRO  = RO
  { roNames = emptyResolvedNames
  , roLevel = 0
  }

newtype Scope a = Scope
  { unScope :: ReaderT RO Dang a
  } deriving (Functor,Applicative,Monad,MonadPlus)

instance BaseM Scope Dang where
  inBase = Scope . inBase

runScope :: Scope a -> Dang a
runScope m = runM (unScope m) emptyRO

withResolved :: ResolvedNames -> Scope a -> Scope a
withResolved names m = Scope $ do
  ro <- ask
  local (ro { roNames = names `mergeResolvedNames` roNames ro }) (unScope m)

bindLocals :: [Name] -> Scope a -> Scope a
bindLocals ns m = Scope $ do
  ro <- ask
  let bound = CM.fromList [ (UsedTerm (simpleName n), Bound n) | n <- ns ]
  local (ro { roNames = bound `mergeResolvedNames` roNames ro }) (unScope m)

bindGlobals :: [QualName] -> Scope a -> Scope a
bindGlobals qns m = Scope $ do
  ro <- ask
  let resolved = CM.fromList [ (UsedTerm qn, Resolved (roLevel ro) qn)
                             | qn <- qns ]
  local (ro { roNames = resolved `mergeResolvedNames` roNames ro }) (unScope m)

newLevel :: Scope a -> Scope a
newLevel m = Scope $ do
  ro <- ask
  local (ro { roLevel = roLevel ro + 1 }) (unScope m)

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
    Resolved _ rqn -> return rqn
    Bound _        -> fail "Unknown bound variable in type"

resolveTerm :: QualName -> Scope Term
resolveTerm qn = do
  r <- resolve (UsedTerm qn)
  case r of
    Resolved _ rqn -> return (Global rqn)
    Bound n        -> return (Local n)

resolveConstrName :: QualName -> Scope QualName
resolveConstrName qn = do
  r <- resolve (UsedTerm qn)
  case r of
    Resolved _ rqn -> return rqn
    _              -> fail "Unknown bound variable in constructor name"


-- Modules ---------------------------------------------------------------------

scopeCheckModule :: Module -> Scope (InterfaceSet,Module)
scopeCheckModule m = do
  logDebug "used symbols"
  let uses = getUses m
  logDebug (show uses)

  logInfo "minimal imports"
  let needed = Set.delete (modName m) (minimalImports uses)
  logInfo (pretty needed)

  logInfo "opening interfaces"
  iset <- loadInterfaces needed

  logDebug "resolved uses"
  let modNames = modResolvedNames m
  let resolved = resolveUses iset uses `mergeResolvedNames` modNames
  logDebug (show resolved)

  m' <- withResolved resolved (scPass m)

  return (iset,m')


-- Generic Scope Checking ------------------------------------------------------

scPass :: Data a => a -> Scope a
scPass  =
  gmapM scPass
  `extM` scMatch
  `extM` scType
  `extM` scTerm

  -- special cases where the traversal needs to ignore kinds
  `extM` scTParam
  `extM` scDataDecl

-- Data Declarations -----------------------------------------------------------

-- | The scope-checking process must skip kinds, as they're built-in to the
-- compiler, and have no origin.
scDataDecl :: DataDecl -> Scope DataDecl
scDataDecl dd = do
  gs <- scPass (dataGroups dd)
  return dd { dataGroups = gs }


-- Types -----------------------------------------------------------------------

scType :: Type -> Scope Type
scType ty = case ty of

  TApp l r -> do
    l' <- scType l
    r' <- scType r
    return (TApp l' r')

  TInfix qn l r -> do
    qn' <- resolveType qn
    l'  <- scType l
    r'  <- scType r
    return (TInfix qn' l' r')

  TCon qn -> TCon <$> resolveType qn

  TVar _ -> return ty

-- | The scope-checking process must skip kinds, as they're built-in to the
-- compiler, and have no origin.
scTParam :: TParam Kind -> Scope (TParam Kind)
scTParam  = return


-- Terms -----------------------------------------------------------------------

scMatch :: Match -> Scope Match
scMatch m = case m of
  MPat p m' -> scPat p (\p' -> MPat p' <$> scMatch m')
  _         -> gmapM scPass m

scPat :: Pat -> (Pat -> Scope a) -> Scope a
scPat p k = case p of

  PVar _ -> bindLocals (patVars p) (k p)

  PCon qn ps -> do
    qn' <- resolveConstrName qn
    bindLocals (patVars p) (k (PCon qn' ps))

  PWildcard -> k p

scTerm :: Term -> Scope Term
scTerm tm = case tm of

  Let ts us e -> newLevel $ do
    let tqs = map typedName   ts
        uqs = map untypedName us
    bindLocals (tqs ++ uqs) $ do
      ts' <- scPass ts
      us' <- scPass us
      e'  <- scTerm e
      return (Let ts' us' e')

  Local n   -> resolveTerm (simpleName n)
  Global qn -> resolveTerm qn

  _ -> gmapM scPass tm
