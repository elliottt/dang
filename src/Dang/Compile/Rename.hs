{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Dang.Compile.Rename (
    rename
  ) where

import Dang.Core.AST
import Dang.IO (logInfo,logStage,logDebug)
import Dang.Monad (Dang)
import Dang.ModuleSystem.QualName
import Dang.Utils.Pretty (pretty)

import Control.Applicative (Applicative(..),(<$>))
import Data.Generics (extM,Data(gmapM))
import Data.Maybe (fromMaybe)
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set


-- External Interface ----------------------------------------------------------

rename :: Module -> Dang Module
rename m = do
  logStage "renamer"
  m' <- runRename (renameModule m)
  logInfo "Renaming output:"
  logDebug (show m')
  logInfo (pretty m')
  return m'


-- Renaming Monad --------------------------------------------------------------

type NameMap a = Map.Map a a

data Subst = Subst
  { sGlobals :: NameMap QualName
  }

emptySubst :: Subst
emptySubst  = Subst
  { sGlobals = Map.empty
  }

data Avoid = Avoid
  { avoidSet :: Set.Set QualName
  }

emptyAvoid :: Avoid
emptyAvoid  = Avoid
  { avoidSet = Set.empty
  }

newtype Rename a = Rename
  { unRename :: StateT Avoid (ReaderT Subst Dang) a
  } deriving (Functor,Applicative,Monad)

instance BaseM Rename Dang where
  inBase = Rename . inBase

runRename :: Rename a -> Dang a
runRename m =
  fst `fmap` runReaderT emptySubst (runStateT emptyAvoid (unRename m))

getSubst :: Rename Subst
getSubst  = Rename ask

withSubst :: Subst -> Rename a -> Rename a
withSubst u m = Rename (local u (unRename m))

avoid :: [QualName] -> Rename ()
avoid names = Rename $ do
  a <- get
  set $! a { avoidSet = Set.fromList names `Set.union` avoidSet a }

nextName :: Set.Set QualName -> QualName -> QualName
nextName avoids qn
  | qn `Set.notMember` avoids = qn
  | otherwise                 = loop 0
  where
  ns = qualPrefix qn
  n  = qualSymbol qn
  loop :: Int -> QualName
  loop i
    | qn' `Set.member` avoids = loop (i+1)
    | otherwise               = qn'
    where
    qn' = qualName ns n'
    n'  = n ++ show i

freshName :: QualName -> Rename QualName
freshName qn = Rename $ do
  a <- get
  return $! nextName (avoidSet a) qn

freshNames :: [QualName] -> Rename a -> Rename a
freshNames ns m = do
  let step v = do
        v' <- freshName v
        return (v,v')
  ns' <- mapM step ns
  u   <- getSubst
  avoid (map snd ns')
  withSubst (u { sGlobals = Map.fromList ns' `Map.union` sGlobals u }) m

substGlobal :: QualName -> Rename QualName
substGlobal qn = do
  u <- getSubst
  return (fromMaybe qn (Map.lookup qn (sGlobals u)))



-- Term Renaming ---------------------------------------------------------------

-- | Generic renaming pass
renamePass :: Data a => a -> Rename a
renamePass  =
  gmapM renamePass
  `extM` renameDecl
  `extM` renameMatch
  `extM` renameTerm

renameModule :: Module -> Rename Module
renameModule m = do
  avoid (map declName (modDecls m))
  decls' <- renamePass (modDecls m)
  return m
    { modDecls = decls'
    }

renameDecl :: Decl -> Rename Decl
renameDecl d = do
  qn'  <- substGlobal (declName d)
  body <- renamePass (declBody d)
  return d
    { declName = qn'
    , declBody = body
    }

renameMatch :: Match -> Rename Match
renameMatch m = case m of

  MPat p m' -> do
    avoid (map simpleName (patVars p))
    MPat p <$> renameMatch m'

  _ -> gmapM renamePass m

-- | As only global variables need to be unique, only let expressions and
-- globals need to be considered for renaming.
renameTerm :: Term -> Rename Term
renameTerm tm = case tm of
  Let ds e  -> renameLet ds e
  Global qn -> Global <$> substGlobal qn
  _         -> gmapM renamePass tm

-- | Introduce globally unique names for let-bound declarations.
renameLet :: [Decl] -> Term -> Rename Term
renameLet ds e = freshNames (map declName ds)
               $ Let <$> mapM renameDecl ds <*> renameTerm e
