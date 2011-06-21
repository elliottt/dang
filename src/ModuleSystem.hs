{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ModuleSystem (
    Scope()
  , runScope
  , scopeCheck
  , moduleImports
  ) where

import Dang.IO
import Dang.Monad
import Interface
import Pretty
import QualName
import ReadWrite
import Syntax.AST
import qualified Data.ClashMap as CM

import Data.Maybe (mapMaybe,catMaybes)
import MonadLib
import qualified Data.Set as Set


-- Scope Checking Monad --------------------------------------------------------

data RO = RO
  { roNames :: ResolvedNames
  }

emptyRO :: RO
emptyRO  = RO
  { roNames = CM.empty
  }

newtype Scope a = Scope
  { getScope :: ReaderT RO Dang a
  } deriving (Functor,Monad)

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


-- Resolved Names --------------------------------------------------------------

data Resolved
  = Resolved QualName
  | Bound Name
    deriving (Eq,Show)

type ResolvedNames = CM.ClashMap QualName Resolved

-- | True when the Resolved name is a bound variable.
isBound :: Resolved -> Bool
isBound Bound{} = True
isBound _       = False

-- | Merge resolved names, favoring new bound variables for shadowing.
mergeResolved :: CM.Strategy Resolved
mergeResolved a b
  | isBound a = CM.ok a
  | a == b    = CM.ok a
  | otherwise = CM.clash a b

-- | Merge two resolved name substitutions.
mergeResolvedNames :: ResolvedNames -> ResolvedNames -> ResolvedNames
mergeResolvedNames  = CM.unionWith mergeResolved


-- Scope Checking --------------------------------------------------------------

-- | Given a qualified name, generate the term that corresponds to its binding.
resolve :: QualName -> Scope Term
resolve qn = do
  ro <- ask
  case CM.clashElems `fmap` CM.lookup qn (roNames ro) of
    Just [r] -> return (resolvedTerm r)
    Just _rs -> fail ("Symbol `" ++ pretty qn ++ "' is defined multiple times")
    Nothing  -> fail ("Symbol `" ++ pretty qn ++ "' not defined")

-- | The term that this entry represents (global/local).
resolvedTerm :: Resolved -> Term
resolvedTerm (Resolved qn) = Global qn
resolvedTerm (Bound n)     = Local n




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
moduleImports :: Module -> Set.Set Use'
moduleImports m =
  Set.fromList (map Explicit (modOpens m)) `Set.union` Set.map Implicit implicit
  where
  env      = importSet (modOpens m)
  implicit = pruneImplicit env (imports m)

data Use' = Explicit Open | Implicit QualName
    deriving (Eq,Show,Ord)

-- | Collect all the implicit module imports.
class Imports a where
  imports :: a -> Set.Set QualName

instance Imports a => Imports (Maybe a) where
  imports = maybe Set.empty imports

instance Imports a => Imports [a] where
  imports = Set.unions . map imports

instance Imports Module where
  imports m = imports (modDecls m)

instance Imports Decl where
  imports d = imports (declBody d)

instance Imports Term where
  imports (Abs _ b)   = imports b
  imports (Let ds b)  = imports ds `Set.union` imports b
  imports (App f xs)  = imports f `Set.union` imports xs
  imports (Local _)   = Set.empty
  imports (Global qn) = maybe Set.empty Set.singleton (qualModule qn)
  imports (Prim _)    = Set.empty
  imports (Lit l)     = imports l

instance Imports Literal where
  imports (LInt _) = Set.empty




-- | The use of a module, via an open declaration, or qualified use.  When the
-- use is from a qualified name only, the boolean value is True, and the module
-- usage is thought of as weak.  The usage is weak in the sense that the
-- qualified name could be from either a real module or a renamed one.  If
-- loading the interface from a weak module fails, that error is ignored,
-- assuming that either the reference will be resolved later through a more
-- specific import, or the symbol will go unresolved, and raised to the
-- programmer.
data Use = Use
  { useOpen :: Open
  , _useWeak :: Bool
  } deriving (Show,Eq,Ord)

-- | Given a module, return the modules that are opened by it.
openedModules :: Module -> Set.Set Use
openedModules m = Set.fromList (map step (modOpens m))
  where
  step o = Use o False

-- | Given a module, return the modules that are referenced in its identifiers,
-- removing fully-qualified references to local identifiers.
implicitlyOpenedModules :: Module -> Set.Set Use
implicitlyOpenedModules m =
  Set.fromList (mapMaybe toUse (Set.toList (identifiers (modDecls m))))
  where
  toUse (QualName ps i)
    | ps == thisModule = Nothing
    | len > 0          = Just (Use (Open qn (Just qn) False [i]) True)
      where
      qn       = qualName ns n
      len      = length ps
      (ns,[n]) = splitAt (len - 1) ps
  toUse _  = Nothing
  thisModule  = modNamespace m

-- | Register all of the names defined in a module as both their local and fully
-- qualified versions.
definedNames :: Module -> ResolvedNames
definedNames m = CM.fromList (concatMap step (modDecls m))
  where
  ns     = modNamespace m
  step d = [ (qn, Resolved qn), (simpleName n, Resolved qn) ]
    where
    n  = declName d
    qn = qualName ns n

-- | Run a scope checking operation with the environment created by a module.
withEnv :: Module -> Scope a -> Scope (Interface R, a)
withEnv m k = do
  let opened = openedModules m `Set.union` implicitlyOpenedModules m
  logDebug "Opened modules:"
  logDebug (show opened)

  let need = Set.toList (uniqueModules opened)
  logDebug "Needed interfaces:"
  logDebug (show need)
  iface <- loadInterfaces need

  let env0 = mergeResolvedNames (definedNames m)
           $ buildEnv iface (Set.toList opened)
  logDebug "Module environment:"
  logDebug (show env0)

  ro  <- ask
  res <- local (ro { roNames = env0 }) k
  return (iface,res)

-- | A requirement for a module interface to be loaded.  The boolean parameter
-- represents a weak need, in which the need is generated from a fully qualified
-- reference to a module member.  When two needs for the same module are
-- interpreted together, one strong and one weak, the strong one will take
-- precedence over the weak one.
data NeedInterface = NeedInterface
  { neededModule :: QualName
  , neededWeak   :: Bool
  } deriving (Show,Eq,Ord)

-- | The set of unique modules that are referenced by set of module uses.
-- Modules that are referenced by a fully qualified use, and imported through an
-- open declaration will be dominated by the open declaration, thus a module
-- need will only be weak if it is only used through a fully qualified
-- reference.
uniqueModules :: Set.Set Use -> Set.Set NeedInterface
uniqueModules uses = strong `Set.union` weak'
  where
  step (Use o isWeak) = NeedInterface (openMod o) isWeak
  (weak,strong)       = Set.partition neededWeak (Set.map step uses)
  strongMods          = Set.map neededModule strong
  weak'               = Set.filter (not . covered) weak
  covered w           = Set.member (neededModule w) strongMods


-- | Given all the interface obligations generated by the initial analysis, load
-- the interfaces from disk.  If an interface obligation was weak, and failed to
-- find an interface file, no error will be reported.
loadInterfaces :: [NeedInterface] -> Scope (Interface R)
loadInterfaces  = foldM step (freezeInterface emptyInterface)
  where
  step i (NeedInterface qn isWeak) = do
    e <- try (inBase (openInterface qn))
    case e of
      Left err | isWeak    -> logInfo ("Ignoring: " ++ show err) >> return i
               | otherwise -> raise err
      Right i'             -> return (mergeInterfaces i i')

-- | Given an aggregate interface, and a set of module uses, generate the final
-- mapping from used names to resolved names.
buildEnv :: Interface R -> [Use] -> ResolvedNames
buildEnv iface = foldr step CM.empty
  where
  step (Use o _) = mergeResolvedNames (resolveOpen iface o)

-- | Resolve an open declaration to the module names that it involves.
resolveOpen :: Interface R -> Open -> ResolvedNames
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
resolveModule :: Interface R -> QualName -> ResolvedNames
resolveModule iface m =
  CM.fromListWith mergeResolved (map step (modContents m iface))
  where
  step (qn,_) = (simpleName (qualSymbol qn), Resolved qn)

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

-- | Fully qualify all of the symbols inside of a module.  This does IO, as it
-- may end up needing to read other interface files to make a decision.
scopeCheck :: Module -> Dang (Interface R, Module)
scopeCheck m = runScope $ do
  logInfo "Running module system"
  withEnv m (scopeCheckModule m)

-- | Check all of the identifiers in a module, requiring that they are defined
-- somewhere.
scopeCheckModule :: Module -> Scope Module
scopeCheckModule m = do
  ds' <- mapM scopeCheckDecl (modDecls m)
  return m
    { modDecls = ds'
    }

-- | Register variables as bound for the computation that is passed.
bindVars :: [Var] -> Scope a -> Scope a
bindVars vs m = do
  ro <- ask
  let locals = CM.fromList [ (simpleName v, Bound v) | v <- vs ]
  local (ro { roNames = mergeResolvedNames locals (roNames ro) }) m

-- | Check all identifiers used in a declaration.
scopeCheckDecl :: Decl -> Scope Decl
scopeCheckDecl d = bindVars (declVars d) $ do
  b' <- scopeCheckTerm (declBody d)
  return d
    { declBody = b'
    }

-- | Check all identifiers used in a term.
scopeCheckTerm :: Term -> Scope Term
scopeCheckTerm t =
  case t of
    Lit _    -> return t
    Prim _   -> return t
    Abs vs b -> Abs vs `fmap` bindVars vs (scopeCheckTerm b)
    Let ds b -> bindVars (map declName ds)
         (Let `fmap` mapM scopeCheckDecl ds `ap` scopeCheckTerm b)
    App f xs -> App `fmap` scopeCheckTerm f `ap` mapM scopeCheckTerm xs
    Global n -> resolve n
    Local n  -> resolve (simpleName n) -- the parser doesn't parse these
