{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compile.Rename (
    rename
  ) where

import Core.AST
import Dang.IO (logInfo,logStage,logDebug)
import Dang.Monad (Dang)
import Pretty (pretty)
import QualName
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
  { sGlobals :: NameMap QualName
  , sLocals  :: NameMap Name
  }

emptySubst :: Subst
emptySubst  = Subst
  { sGlobals = Map.empty
  , sLocals  = Map.empty
  }

data Avoid = Avoid
  { avoidIx  :: !Int
  , avoidSet :: Set.Set QualName
  }

emptyAvoid :: Avoid
emptyAvoid  = Avoid
  { avoidIx  = 0
  , avoidSet = Set.empty
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

freshName :: Name -> Rename Name
freshName n = Rename $ do
  a <- get
  if simpleName n `Set.notMember` avoidSet a
    then return n
    else do
      let loop i
            | qn `Set.member` avoidSet a = loop (i+1)
            | otherwise                  = (name,i+1)
            where
            qn   = simpleName name
            name = n ++ show i

          (n',i') = loop (avoidIx a)
      set $! a { avoidIx = i' }
      return n'


freshLocals :: [QualName] -> Rename a -> Rename a
freshLocals ns m = do
  let step v = do
        let sym = qualSymbol v
        n <- freshName sym
        return (sym,n)
  ns' <- mapM step ns
  u   <- getSubst
  avoid (map (simpleName . snd) ns')
  withSubst (u { sLocals = Map.fromList ns' `Map.union` sLocals u }) m

substGlobal :: QualName -> Rename QualName
substGlobal qn = do
  u <- getSubst
  return (fromMaybe qn (Map.lookup qn (sGlobals u)))

substLocal :: Name -> Rename Name
substLocal n = do
  u <- getSubst
  return (fromMaybe n (Map.lookup n (sLocals u)))



-- Term Renaming ---------------------------------------------------------------

renameModule :: Module -> Rename Module
renameModule m = do
  avoid (map declName (modDecls m))
  decls' <- mapM renameDecl (modDecls m)
  return m
    { modDecls = decls'
    }

renameDecl :: Decl -> Rename Decl
renameDecl d = do
  qn'  <- substGlobal (declName d)
  body <- renameForall renameMatch (declBody d)
  return d
    { declName = qn'
    , declBody = body
    }

renameForall :: (a -> Rename a) -> Forall a -> Rename (Forall a)
renameForall k (Forall ps a) = Forall ps `fmap` k a

renameMatch :: Match -> Rename Match
renameMatch m = case m of
  MPat p m'   -> do
    avoid (map simpleName (patVars p))
    MPat p <$> renameMatch m'
  MSplit l r  -> MSplit <$> renameMatch l <*> renameMatch r
  MTerm tm ty -> MTerm  <$> renameTerm tm <*> pure ty
  MFail _     -> pure m

renameTerm :: Term -> Rename Term
renameTerm tm = case tm of
  AppT f tys -> AppT   <$> renameTerm f <*> pure tys
  App  f xs  -> App    <$> renameTerm f <*> mapM renameTerm xs
  Case e m   -> Case   <$> renameTerm e <*> renameMatch m
  Let ds e   -> renameLet ds e
  Global qn  -> Global <$> substGlobal qn
  Local n    -> Local  <$> substLocal  n
  Lit{}      -> return tm

renameLet :: [Decl] -> Term -> Rename Term
renameLet ds e = freshLocals (map declName ds)
               $ Let <$> mapM renameLetDecl ds <*> renameTerm e

renameLetDecl :: Decl -> Rename Decl
renameLetDecl d = do
  n'   <- substLocal (qualSymbol (declName d))
  body <- renameForall renameMatch (declBody d)
  return d
    { declName = simpleName n'
    , declBody = body
    }
