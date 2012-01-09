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
    ,DataDecl(..),ConstrGroup(..),Constr(..),Term(..),Pat(..))
import TypeChecker.Types (Type(..),Forall(..))
import qualified Data.ClashMap as CM

import Control.Applicative (Applicative,(<$>),(<*>))
import MonadLib
import qualified Data.Set as Set


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

  m' <- withResolved resolved (scModule m)

  return (iset,m')

scModule :: Module -> Scope Module
scModule m = do
  ptms <- mapM scPrimTerm    (modPrimTerms m)
  ts   <- mapM scTypedDecl   (modTyped m)
  us   <- mapM scUntypedDecl (modUntyped m)
  ds   <- mapM scDataDecl    (modDatas m)
  return m
    { modPrimTerms = ptms
    , modTyped     = ts
    , modUntyped   = us
    , modDatas     = ds
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
scDataDecl dd = do
  gs <- mapM (scForall scConstrGroup) (dataGroups dd)
  return dd { dataGroups = gs }

scConstrGroup :: ConstrGroup -> Scope ConstrGroup
scConstrGroup cg = do
  tys <- mapM scType (groupArgs cg)
  cs  <- mapM scConstr (groupConstrs cg)
  return cg
    { groupArgs    = tys
    , groupConstrs = cs
    }

scConstr :: Constr -> Scope Constr
scConstr c = do
  fs <- mapM scType (constrFields c)
  return c { constrFields = fs }


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
scMatch m = case m of
  MPat p m' -> scPat p (\p' -> MPat p' <$> scMatch m')
  MTerm tm  -> MTerm <$> scTerm tm

scPat :: Pat -> (Pat -> Scope a) -> Scope a
scPat p k = case p of
  PWildcard -> k p
  PVar n    -> bindLocals [n] (k p)

scTerm :: Term -> Scope Term
scTerm tm = case tm of
  Abs m -> Abs <$> scMatch m

  Let ts us e -> newLevel $ do
    let tqs = map (simpleName . typedName)   ts
        uqs = map (simpleName . untypedName) us
    bindGlobals (tqs ++ uqs) $ do
      ts' <- mapM scTypedDecl ts
      us' <- mapM scUntypedDecl us
      e'  <- scTerm e
      return (Let ts' us' e')

  App f xs -> App <$> scTerm f <*> mapM scTerm xs

  Local n   -> resolveTerm (simpleName n)
  Global qn -> resolveTerm qn

  Lit _ -> return tm
