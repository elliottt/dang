{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ModuleSystem where

import Dang.IO
import Dang.Monad
import Interface
import Pretty
import QualName
import ReadWrite
import Syntax.AST

import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable)
import MonadLib
import qualified Data.Set as Set
import qualified Data.Map as Map

data Resolved
  = Resolved QualName
  | Bound Name
  | Clash Name [QualName]
    deriving Show

resolvedNames :: Resolved -> [QualName]
resolvedNames (Resolved qn) = [qn]
resolvedNames (Bound n)     = [simpleName n]
resolvedNames (Clash _ ns)  = ns

-- | Merge resolved names, favoring new bound variables for shadowing.
mergeResolved :: Resolved -> Resolved -> Resolved
mergeResolved a@Bound{} _ = a
mergeResolved a         b = clash a b

-- | Merge two resolved names into a name clash.
clash :: Resolved -> Resolved -> Resolved
clash a@(Clash n _)   b = Clash n (resolvedNames a ++ resolvedNames b)
clash   (Bound n)     b = Clash n (simpleName n : resolvedNames b)
clash   (Resolved qn) b = Clash (qualSymbol qn) (qn : resolvedNames b)

-- | The term that this entry represents (global/local).
resolvedTerm :: Resolved -> Scope Term
resolvedTerm (Resolved qn) = return (Global qn)
resolvedTerm (Bound n)     = return (Local n)
resolvedTerm (Clash n ns)  =
  fail (n ++ " defined in multiple places:" ++ unlines (map pretty ns))

type ResolvedNames = Map.Map QualName Resolved

addResolvedName :: QualName -> QualName -> ResolvedNames -> ResolvedNames
addResolvedName n rn = Map.insertWith mergeResolved n (Resolved rn)

mergeResolvedNames :: ResolvedNames -> ResolvedNames -> ResolvedNames
mergeResolvedNames  = Map.unionWith mergeResolved

-- | Add the unqualified names to the resolved names map.
interfaceResolvedNames :: QualName -> Interface R -> ResolvedNames
interfaceResolvedNames m i =
  Map.fromListWith mergeResolved (map step (modContents m i))
  where
  step (qn,_) = (simpleName (qualSymbol qn), Resolved qn)

resolve :: QualName -> Scope Term
resolve qn = do
  ro <- ask
  case Map.lookup qn (roNames ro) of
    Nothing -> fail ("Symbol `" ++ pretty qn ++ "' not defined")
    Just r  -> resolvedTerm r

data RO = RO
  { roNames :: ResolvedNames
  }

emptyRO :: RO
emptyRO  = RO
  { roNames = Map.empty
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

runScope :: Scope a -> Dang a
runScope  = runReaderT emptyRO . getScope

data NotInScope = NotInScope QualName
    deriving (Show,Typeable)

instance Exception NotInScope

notFound :: QualName -> Scope a
notFound  = raiseE . NotInScope

data Use
  = QualIdent QualName Name
  | OpenModule QualName
    deriving (Eq,Ord,Show)

useModule :: Use -> QualName
useModule (QualIdent qn _) = qn
useModule (OpenModule qn)  = qn

-- | Given a module, find all of the modules that it uses.
usedModules :: Module -> Set.Set Use
usedModules m = openModules m `Set.union` identModules m

uniqueModules :: Set.Set Use -> Set.Set QualName
uniqueModules  = Set.map useModule

-- | Given a module, return the modules that are referenced in its identifiers.
identModules :: Module -> Set.Set Use
identModules m =
  Set.fromList (mapMaybe toModule (Set.toList (identifiers (modDecls m))))
  where
  toModule (QualName ps i)
    | ps == thisModule = Nothing
    | len > 0          = Just (QualIdent (qualName ns n) i)
      where
      len      = length ps
      (ns,[n]) = splitAt (len - 1) ps
  toModule _  = Nothing
  thisModule  = modNamespace m

-- | Given a module, return the modules that are opened by it.
openModules :: Module -> Set.Set Use
openModules m = Set.fromList (map (OpenModule . openMod) (modOpens m))

-- | Run a scope checking operation with the environment created by a module.
withEnv :: Module -> Scope a -> Scope (Interface R, a)
withEnv m k = do
  let uses = usedModules m
  logDebug "Used modules"
  logDebug (show uses)
  iface <- loadInterfaces (Set.toList (uniqueModules uses))
  let env0   = buildEnv iface (Set.toList uses)
      ns     = modNamespace m
      locals = Map.fromList [ (simpleName n, Resolved (qualName ns n))
                            | d <- modDecls m, let n = declName d ]
  ro  <- ask
  res <- local (ro { roNames = mergeResolvedNames locals env0 }) k
  return (iface,res)

loadInterfaces :: [QualName] -> Scope (Interface R)
loadInterfaces  = foldM step (freezeInterface emptyInterface)
  where
  step i qn = mergeInterfaces i `fmap` inBase (openInterface qn)

buildEnv :: Interface R -> [Use] -> ResolvedNames
buildEnv iface = foldr step Map.empty
  where
  step (OpenModule m)  = mergeResolvedNames (interfaceResolvedNames m iface)
  step (QualIdent m n) = addResolvedName name name
    where
    name = qualName (qualPrefix m ++ [qualSymbol m]) n

-- | Fully qualify all of the symbols inside of a module.  This does IO, as it
-- may end up needing to read other interface files to make a decision.
scopeCheck :: Module -> Dang (Interface R, Module)
scopeCheck m = runScope $ do
  logInfo "Running module system"
  withEnv m (scopeCheckModule m)

scopeCheckModule :: Module -> Scope Module
scopeCheckModule m = do
  ds' <- mapM scopeCheckDecl (modDecls m)
  return m
    { modDecls = ds'
    }

bindVars :: [Var] -> Scope a -> Scope a
bindVars vs m = do
  ro <- ask
  let locals = Map.fromList [ (simpleName v, Bound v) | v <- vs ]
  local (ro { roNames = mergeResolvedNames locals (roNames ro) }) m

scopeCheckDecl :: Decl -> Scope Decl
scopeCheckDecl d = bindVars (declVars d) $ do
  b' <- scopeCheckTerm (declBody d)
  return d
    { declBody = b'
    }

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
