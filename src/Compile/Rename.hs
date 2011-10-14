{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compile.Rename (
    rename
  ) where

import Dang.IO (logInfo,logStage,logDebug)
import Dang.Monad (Dang)
import Pretty (pretty)
import QualName
import TypeChecker.AST
import TypeChecker.Types (Forall(..))

import Control.Applicative (Applicative(..),(<$>))
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
  { sNames :: NameMap QualName
  , sAvoid :: Set.Set QualName
  }

emptySubst :: Subst
emptySubst  = Subst
  { sNames = Map.empty
  , sAvoid = Set.empty
  }

newtype Rename a = Rename
  { unRename :: StateT Int (ReaderT Subst Dang) a
  } deriving (Functor,Applicative,Monad)

instance BaseM Rename Dang where
  inBase = Rename . inBase

runRename :: Rename a -> Dang a
runRename m = fst `fmap` runReaderT emptySubst (runStateT 0 (unRename m))

getSubst :: Rename Subst
getSubst  = Rename ask

withSubst :: Subst -> Rename a -> Rename a
withSubst u m = Rename (local u (unRename m))

avoid :: [QualName] -> Rename a -> Rename a
avoid names m = do
  u <- getSubst
  withSubst (u { sAvoid = Set.fromList names `Set.union` sAvoid u }) m

freshName :: String -> Rename QualName
freshName pfx = do
  u  <- getSubst
  i0 <- Rename get
  let bound = sAvoid u
      loop i
        | name `Set.member` bound = loop (i+1)
        | otherwise               = (name,i+1)
        where
        name = simpleName (pfx ++ show i)

      (n,i') = loop i0
  Rename (set $! i')
  return n

fresh :: String -> [QualName] -> Rename a -> Rename a
fresh pfx ns m = do
  let step v = do
        n <- freshName pfx
        return (v,n)
  ns' <- mapM step ns
  u   <- getSubst
  avoid (map snd ns')
    (withSubst (u { sNames = Map.fromList ns' `Map.union` sNames u }) m)

subst :: QualName -> Rename QualName
subst qn = do
  u <- getSubst
  return (fromMaybe qn (Map.lookup qn (sNames u)))


-- Term Renaming ---------------------------------------------------------------

renameModule :: Module -> Rename Module
renameModule m = avoid (map declName (modDecls m)) $ do
  decls' <- mapM renameDecl (modDecls m)
  return m
    { modDecls = decls'
    }

renameDecl :: Decl -> Rename Decl
renameDecl d = do
  qn'  <- subst (declName d)
  body <- renameForall renameMatch (declBody d)
  return d
    { declName = qn'
    , declBody = body
    }

renameForall :: (a -> Rename a) -> Forall a -> Rename (Forall a)
renameForall k (Forall ps a) = Forall ps `fmap` k a

renameMatch :: Match -> Rename Match
renameMatch m = case m of
  MPat p m'   -> avoid (map simpleName (patVars p))
               $ MPat p <$> renameMatch m'
  MTerm tm ty -> MTerm  <$> renameTerm tm <*> pure ty

renameTerm :: Term -> Rename Term
renameTerm tm = case tm of
  AppT f tys -> AppT    <$> renameTerm f <*> pure tys
  App  f xs  -> App     <$> renameTerm f <*> mapM renameTerm xs
  Let ds e   -> fresh "_decl" (map declName ds)
              $ Let    <$> mapM renameDecl ds <*> renameTerm e
  Global qn  -> Global <$> subst qn
  Local n    -> return (Local n)
  Lit lit    -> return (Lit lit)
