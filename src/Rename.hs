{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Rename (
    Rename()
  , runRename
  , renameDecls
  ) where

import AST

import Control.Applicative (Applicative(..),(<$>))
import MonadLib
import qualified Data.Map as Map


-- Renaming Monad --------------------------------------------------------------

data RO = RO
  { roSubst :: Map.Map Var Var
  }

addSubst :: Var -> Var -> RO -> RO
addSubst a b ro = ro { roSubst = Map.insert a b (roSubst ro) }

captures :: RO -> Var -> Bool
captures ro v = Map.member v (roSubst ro)

newtype Rename m a = Rename
  { unRename :: StateT Int (ReaderT RO m) a
  } deriving (Functor,Applicative,Monad)

runRename :: Monad m => [Var] -> Rename m a -> m a
runRename bound (Rename m) = do
  let ro = RO
        { roSubst = Map.fromList [ (x,x) | x <- bound ]
        }
  (a,_) <- runReaderT ro (runStateT 0 m)
  return a

instance Monad m => StateM (Rename m) Int where
  get = Rename get
  set = Rename . set

instance Monad m => ReaderM (Rename m) RO where
  ask = Rename ask

instance Monad m => RunReaderM (Rename m) RO where
  local ro = Rename . local ro . unRename

subst :: Monad m => Var -> Rename m Var
subst v = do
  ro <- ask
  case Map.lookup v (roSubst ro) of
    Nothing -> return v
    Just x  -> return x

-- | Introduce a fresh name into a renaming context.
intro :: Monad m => (Var -> Rename m a) -> Rename m a
intro k = do
  i  <- get
  ro <- ask
  let (ro',i',[v]) = findFresh ro i [""]
  set i'
  local ro' (k v)

-- | Give fresh names to all that are passed, in the context of the given
-- action.
fresh :: Monad m => [Var] -> Rename m a -> Rename m a
fresh vs m = do
  i  <- get
  ro <- ask
  let (ro',i',_) = findFresh ro i vs
  set i'
  local ro' m

findFresh :: RO -> Int -> [Var] -> (RO,Int,[Var])
findFresh  = loop []
  where
  loop rs ro i []      = (ro, i,reverse rs)
  loop rs ro i (v:vs)
    | ro `captures` v' = loop     rs                 ro  i' (v:vs)
    | otherwise        = loop (v':rs) (addSubst v v' ro) i'    vs
    where
    i' = i + 1
    v' = "_" ++ v ++ show i



-- Term Renaming ---------------------------------------------------------------

-- | Rename declarations, assuming that names are already present in the
-- environment.
renameDecls :: Monad m => [Decl] -> Rename m [Decl]
renameDecls  = mapM renameDecl

-- | Rename a declaration, assuming a fresh name is already in the environment.
renameDecl :: Monad m => Decl -> Rename m Decl
renameDecl d = do
  n' <- subst (declName d)
  fresh (declVars d) $ do
    vs' <- mapM subst (declVars d)
    b'  <- renameTerm (declBody d)
    return d
      { declName = n'
      , declVars = vs'
      , declBody = b'
      }

-- | Rename variable occurrences and bindings in terms.
renameTerm :: Monad m => Term -> Rename m Term
renameTerm t =
  case t of
    App f xs -> apply <$> renameTerm f <*> mapM renameTerm xs
    Var v    -> Var   <$> subst v
    Lit l    -> Lit   <$> renameLiteral l
    Let ds e ->
      fresh (map declName ds) (Let <$> mapM renameDecl ds <*> renameTerm e)
    Abs vs b -> intro $ \name -> fresh vs $ do
      vs' <- mapM subst vs
      b'  <- renameTerm b
      return (Let [Decl name vs' False b'] (Var name))


-- | Rename literals.  For the time being, this doesn't do anything.
renameLiteral :: Monad m => Literal -> Rename m Literal
renameLiteral (LInt i) = return (LInt i)
