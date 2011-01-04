{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ModuleSystem where

import Dang.IO
import Dang.Monad
import Interface
import QualName
import ReadWrite
import Syntax.AST

import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable)
import MonadLib
import qualified Data.Set as Set

data RO = RO
  { roBound     :: Set.Set Var
  , roInterface :: Interface R
  }

emptyRO :: RO
emptyRO  = RO
  { roBound     = Set.empty
  , roInterface = freezeInterface emptyInterface
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

getInterface :: Scope (Interface R)
getInterface  = roInterface `fmap` ask

withInterfaces :: Module -> Scope a -> Scope a
withInterfaces m k = do
  let mods = usedModules m
  logDebug "Used modules"
  logDebug (show mods)
  iface <- loadInterfaces (Set.toList (uniqueModules mods))
  ro    <- ask
  local (ro { roInterface = iface }) k

loadInterfaces :: [QualName] -> Scope (Interface R)
loadInterfaces  = foldM step (freezeInterface emptyInterface)
  where
  step i qn = mergeInterfaces i `fmap` inBase (openInterface qn)

-- | Fully qualify all of the symbols inside of a module.  This does IO, as it
-- may end up needing to read other interface files to make a decision.
scopeCheck :: Module -> Dang (Interface R, Module)
scopeCheck m = runScope $ do
  logInfo "Running module system"
  withInterfaces m $ do
    m'    <- scopeCheckModule m
    iface <- getInterface
    return (iface, m')

scopeCheckModule :: Module -> Scope Module
scopeCheckModule m = do
  ds' <- mapM scopeCheckDecl (modDecls m)
  return m
    { modDecls = ds'
    }

bindVars :: [Var] -> Scope a -> Scope a
bindVars vs m = do
  ro <- ask
  local (ro { roBound = Set.fromList vs `Set.union` roBound ro }) m

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
    Global n -> scopeCheckQualName n
    Local n  -> scopeCheckQualName (simpleName n)

scopeCheckQualName :: QualName -> Scope Term
scopeCheckQualName qn = do
  ro <- ask
  let n = qualSymbol qn
  let checkSimple
        | Set.member n (roBound ro) = return (Local n)
        | otherwise                 =
          case findUnqualFunDecl n (roInterface ro) of
            []        -> notFound qn
            [(qn',_)] -> return (Global qn')
            _         -> fail ("Too many bindings for: " ++ n)
  let checkGlobal =
        case findFunDecl qn (roInterface ro) of
          Nothing -> notFound qn
          Just _  -> return (Global qn)
  if isSimpleName qn
     then checkSimple
     else checkGlobal
