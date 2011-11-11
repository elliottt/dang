{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ModuleSystem (
    Scope()
  , runScope
  , scopeCheck
  ) where

import Dang.IO
import Dang.Monad
import Interface
import ModuleSystem.Imports (getUses,minimalImports)
import Pretty
import QualName
import Syntax.AST
import TypeChecker.Types
import qualified Data.ClashMap as CM

import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Typeable (Typeable)
import MonadLib
import qualified Data.Foldable as F
import qualified Data.Set as Set


-- Scope Checking Monad --------------------------------------------------------

type Level = Int

data RO = RO
  { roNames :: ResolvedNames
  , roLevel :: Level
  }

emptyRO :: RO
emptyRO  = RO
  { roNames = CM.empty
  , roLevel = 0
  }

newtype Scope a = Scope
  { getScope :: ReaderT RO Dang a
  } deriving (Functor,Applicative,Monad)

instance ReaderM Scope RO where
  ask = Scope ask

instance RunReaderM Scope RO where
  local i m = Scope (local i (getScope m))

instance BaseM Scope Dang where
  inBase = Scope . inBase

instance ExceptionM Scope SomeException where
  raise = Scope . raise

instance RunExceptionM Scope SomeException where
  try = Scope . try . getScope

runScope :: Scope a -> Dang a
runScope  = runReaderT emptyRO . getScope


-- Errors ----------------------------------------------------------------------

data ScopeError = MissingInterface QualName
    deriving (Show,Typeable)

instance Exception ScopeError

-- | Generate a missing interface exception, with the module name that was being
-- loaded.
missingInterface :: QualName -> Scope a
missingInterface  = raiseE . MissingInterface


-- Import Gathering ------------------------------------------------------------

type ImportSet = Set.Set QualName

-- | Generate the set of module names that are affected by open declarations.
importSet :: [Open] -> ImportSet
importSet opens
  = Set.fromList
  $ catMaybes $ concat [ [Just (openMod m), openAs m] | m <- opens ]

-- | Given a @ImportSet@ and a set of implicit imports via qualified names,
-- produce a set of module names to be imported.
pruneImplicit :: ImportSet -> Set.Set QualName -> Set.Set QualName
pruneImplicit renames = (Set.\\ renames)

-- | Generate the set of module interfaces that require loading, from a
-- @Module@.
moduleImports :: Module -> Set.Set Use
moduleImports m =
  Set.fromList (map Explicit (modOpens m)) `Set.union` Set.map Implicit implicit
  where
  env      = importSet (modOpens m)
  implicit = pruneImplicit env (imports m)

data Use = Explicit Open | Implicit QualName
    deriving (Eq,Show,Ord)

usedModule :: Use -> QualName
usedModule (Explicit o)  = openMod o
usedModule (Implicit qn) = qn

-- | Collect all the implicit module imports.
class Imports a where
  imports :: a -> Set.Set QualName

instance Imports a => Imports (Maybe a) where
  imports = maybe Set.empty imports

instance Imports a => Imports [a] where
  imports = Set.unions . map imports

instance Imports Module where
  imports m = imports (modTyped m) `Set.union` imports (modUntyped m)

instance Imports TypedDecl where
  imports = imports . typedBody

instance Imports UntypedDecl where
  imports = imports . untypedBody

instance Imports Term where
  imports (Abs m)       = imports m
  imports (Let ts us b) = Set.unions [imports ts, imports us, imports b]
  imports (App f xs)    = imports f `Set.union` imports xs
  imports (Local _)     = Set.empty
  imports (Global qn)   = maybe Set.empty Set.singleton (qualModule qn)
  imports (Lit l)       = imports l

instance Imports Match where
  imports (MPat _ m) = imports m
  imports (MTerm b)  = imports b

instance Imports Literal where
  imports (LInt _) = Set.empty

-- | Run a scope checking operation with the environment created by a module.
withEnv :: Module -> Scope a -> Scope (InterfaceSet, a)
withEnv m k = do
  let opened = moduleImports m
  logDebug "Opened modules:"
  logDebug (show opened)

  let t  = getUses m
      is = minimalImports t
  logInfo "test:"
  logInfo (show t)
  logInfo (show is)

  let need = Set.toList opened
  logDebug "Needed interfaces:"
  logDebug (show need)
  iface <- loadInterfaces opened

  let env0 = mergeResolvedNames (definedNames m) (buildEnv iface opened)
  logDebug "Module environment:"
  logDebug (show env0)

  ro  <- ask
  res <- local (ro { roNames = env0 }) k
  return (iface,res)

-- | Register all of the names defined in a module as both their local and fully
-- qualified versions.
definedNames :: Module -> ResolvedNames
definedNames m = CM.fromList
               $ concatMap primTypeNames (modPrimTypes m)
              ++ concatMap primTermNames (modPrimTerms m)
              ++ concatMap (typedNames 0 ns) (modTyped m)
              ++ concatMap (untypedNames 0 ns) (modUntyped m)
  where
  ns = modNamespace m

-- | Extract the simple and qualified names that a declaration introduces.
declResolvedNames :: (a -> Name) -> (Name -> QualName)
                  -> (Level -> a -> [(QualName,Resolved)])
declResolvedNames simple qualify l d = [ (simpleName n, res), (qn, res) ]
  where
  n   = simple d
  qn  = qualify n
  res = Resolved l qn

-- | The resolved names from a single typed declaration.
typedNames :: Level -> Namespace -> TypedDecl -> [(QualName,Resolved)]
typedNames l ns = declResolvedNames typedName (qualName ns) l

untypedNames :: Level -> Namespace -> UntypedDecl -> [(QualName,Resolved)]
untypedNames l ns = declResolvedNames untypedName (qualName ns) l

-- | The resolved names form a single primitive type declaration.
primTypeNames :: PrimType -> [(QualName,Resolved)]
primTypeNames  = declResolvedNames primTypeName primName 0

-- | The resolved names form a single primitive term declaration.
primTermNames :: PrimTerm -> [(QualName,Resolved)]
primTermNames  = declResolvedNames primTermName primName 0

-- | Given all the interface obligations generated by the initial analysis, load
-- the interfaces from disk.  If an interface obligation was weak, and failed to
-- find an interface file, no error will be reported.
loadInterfaces :: Set.Set Use -> Scope InterfaceSet
loadInterfaces  = F.foldlM step emptyInterfaceSet
  where
  step is u = do
    let m = usedModule u
    e <- try (inBase (openInterface m))
    case e of
      Right iface -> return (addInterface iface is)
      Left{}      -> missingInterface m

-- | Given an aggregate interface, and a set of module uses, generate the final
-- mapping from used names to resolved names.
buildEnv :: InterfaceSet -> Set.Set Use -> ResolvedNames
buildEnv iface = F.foldr step CM.empty
  where
  step (Explicit o)  = mergeResolvedNames (resolveOpen iface o)
  step (Implicit qn) = mergeResolvedNames (resolveModule iface qn)

data Resolved
  = Resolved Level QualName
  | Bound Name
    deriving (Eq,Show)

type ResolvedNames = CM.ClashMap QualName Resolved

-- | True when the Resolved name is a bound variable.
isBound :: Resolved -> Bool
isBound Bound{} = True
isBound _       = False

shadows :: Resolved -> Resolved -> Bool
shadows (Resolved i _) (Resolved j _) = i > j
shadows _              _              = False

-- | Merge resolved names, favoring new bound variables for shadowing.
mergeResolved :: CM.Strategy Resolved
mergeResolved a b
  | isBound a     = CM.ok a
  | a == b        = CM.ok a
  | a `shadows` b = CM.ok a
  | otherwise     = CM.clash a b

-- | Merge two resolved name substitutions.
mergeResolvedNames :: ResolvedNames -> ResolvedNames -> ResolvedNames
mergeResolvedNames  = CM.unionWith mergeResolved

resolve :: QualName -> Scope Resolved
resolve qn = do
  ro <- ask
  case CM.clashElems `fmap` CM.lookup qn (roNames ro) of
    Just [r] -> return r
    Just _rs -> fail ("Symbol `" ++ pretty qn ++ "' is defined multiple times")
    Nothing  -> fail ("Symbol `" ++ pretty qn ++ "' not defined")

-- | Given a qualified name, generate the term that corresponds to its binding.
resolveTerm :: QualName -> Scope Term
resolveTerm qn = resolvedTerm `fmap` resolve qn

-- | The term that this entry represents (global/local).
resolvedTerm :: Resolved -> Term
resolvedTerm (Resolved _ qn) = Global qn
resolvedTerm (Bound n)       = Local n

-- | Resolve a constructor name to its qualified version.
resolveType :: QualName -> Scope QualName
resolveType qn = resolvedType =<< resolve qn

-- | The qualified type name that results from a resolved name.
resolvedType :: Resolved -> Scope QualName
resolvedType (Resolved _ qn) = return qn
resolvedType Bound{}         = fail "Unexpected bound variable in type"

-- | Resolve an open declaration to the module names that it involves.
resolveOpen :: InterfaceSet -> Open -> ResolvedNames
resolveOpen iface o = rename resolved
  where
  syms                    = resolveModule iface (openMod o)
  resolved | openHiding o = resolveHiding (openSymbols o) syms
           | otherwise    = resolveOnly   (openSymbols o) syms
  rename = case openAs o of
    Nothing -> id
    Just m' -> CM.mapKeys (changeNamespace (qualNamespace m'))

-- | Resolve all symbols from a module as though they were opened with no
-- qualifications.
resolveModule :: InterfaceSet -> QualName -> ResolvedNames
resolveModule iface m =
  CM.fromListWith mergeResolved (map step (modContents m iface))
  where
  step (qn,_) = (simpleName (qualSymbol qn), Resolved 0 qn)

-- | Resolve an open declaration that hides some names, and optionally renames
-- the module.
resolveHiding :: [Name] -> ResolvedNames -> ResolvedNames
resolveHiding ns syms = foldl step syms ns
  where
  step m n = CM.delete (simpleName n) m

-- | Resolve an open declaration that selects some names, and optionally renames
-- the module.
resolveOnly :: [Name] -> ResolvedNames -> ResolvedNames
resolveOnly ns syms = CM.intersection syms (CM.fromList (map step ns))
  where
  step n = (simpleName n,error "ModuleSystem.resolveOnly")


-- Scope Checking --------------------------------------------------------------

-- | Fully qualify all of the symbols inside of a module.  This does IO, as it
-- may end up needing to read other interface files to make a decision.
scopeCheck :: Module -> Dang (InterfaceSet, Module)
scopeCheck m = do
  logStage "module-system"
  res@(_,scm) <- runScope (withEnv m (scopeCheckModule m))
  logInfo "Module system output"
  logDebug (show scm)
  logInfo (pretty scm)
  return res

-- | Check all of the identifiers in a module, requiring that they are defined
-- somewhere.
scopeCheckModule :: Module -> Scope Module
scopeCheckModule m = do
  pts <- mapM scopeCheckPrimTerm (modPrimTerms m)
  ts  <- mapM scopeCheckTypedDecl (modTyped m)
  us  <- mapM scopeCheckUntypedDecl (modUntyped m)
  return m
    { modPrimTerms = pts
    , modTyped     = ts
    , modUntyped   = us
    }

-- | Register variables as bound for the computation that is passed.
bindLocals :: [Var] -> Scope a -> Scope a
bindLocals vs m = do
  ro <- ask
  let locals = CM.fromList [ (simpleName v, Bound v) | v <- vs ]
  local (ro { roNames = mergeResolvedNames locals (roNames ro) }) m

-- | Register variables as bound, top-level names for the computation that is
-- passed.
bindGlobals :: [Var] -> Scope a -> Scope a
bindGlobals vs m = do
  ro <- ask
  let l'      = roLevel ro + 1
      globals = CM.fromList
        [ (qn, Resolved l' qn) | v <- vs, let qn = simpleName v ]
      ro' = ro
        { roNames = mergeResolvedNames globals (roNames ro)
        , roLevel = l'
        }
  local ro' m

-- | Check all identifiers used in a declaration.
scopeCheckTypedDecl :: TypedDecl -> Scope TypedDecl
scopeCheckTypedDecl d = do
  qt <- scopeCheckForall (typedType d)
  b  <- scopeCheckMatch  (typedBody d)
  return d
    { typedType = qt
    , typedBody = b
    }

-- | Check all identifiers used in a declaration.
scopeCheckUntypedDecl :: UntypedDecl -> Scope UntypedDecl
scopeCheckUntypedDecl d = do
  b  <- scopeCheckMatch (untypedBody d)
  return d { untypedBody = b }

-- | Check the type associated with a primitive term.
scopeCheckPrimTerm :: PrimTerm -> Scope PrimTerm
scopeCheckPrimTerm pt = do
  ty <- scopeCheckForall (primTermType pt)
  return pt { primTermType = ty }

-- | Check all identifiers used in a term.
scopeCheckTerm :: Term -> Scope Term
scopeCheckTerm t = case t of
  Lit _       -> return t
  Abs m       -> Abs <$> scopeCheckMatch m
  Let ts us b -> bindGlobals (letBinds ts us)
               $ Let <$> mapM scopeCheckTypedDecl ts
                     <*> mapM scopeCheckUntypedDecl us
                     <*> scopeCheckTerm b
  App f xs    -> App <$> scopeCheckTerm f <*> mapM scopeCheckTerm xs
  Global n    -> resolveTerm n
  Local n     -> resolveTerm (simpleName n) -- the parser doesn't parse these

-- | Check all identifiers introduced by a @Match@.
scopeCheckMatch :: Match -> Scope Match
scopeCheckMatch m = case m of
  MTerm t   -> MTerm  <$> scopeCheckTerm t
  MPat p m' -> MPat p <$> bindLocals (patVars p) (scopeCheckMatch m')

-- | Check the underlying type in a quantified type.
scopeCheckForall :: Forall Type -> Scope (Forall Type)
scopeCheckForall (Forall ps ty) = Forall ps `fmap` scopeCheckType ty

-- | Check all identifiers used in a type.
scopeCheckType :: Type -> Scope Type
scopeCheckType ty = case ty of
  TApp l r -> TApp <$> scopeCheckType l <*> scopeCheckType r

  TInfix n l r ->
    TInfix <$> resolveType n <*> scopeCheckType l <*> scopeCheckType r

  TCon n -> TCon <$> resolveType n

  TVar{} -> return ty
