{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Rename (
    renameDecls
  , Rename()
  , runRename
  ) where

import AST

import Control.Applicative (Applicative(..),(<$>))
import Data.Graph (SCC(..))
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Renaming Monad --------------------------------------------------------------

data RO = RO
  { roSubst :: Map.Map Var Var
  }

addSubst :: Var -> Var -> RO -> RO
addSubst a b ro = ro { roSubst = Map.insert a b (roSubst ro) }

captures :: RO -> Var -> Bool
captures ro v = Map.member v (roSubst ro)

newtype Rename a = Rename
  { unRename :: StateT Int (ReaderT RO Lift) a
  } deriving (Functor,Applicative,Monad)

runRename :: [Var] -> Rename a -> a
runRename bound = fst . runLift . runReaderT ro . runStateT 0 . unRename
  where
  ro = RO
    { roSubst = Map.fromList [ (x,x) | x <- bound ]
    }

instance StateM Rename Int where
  get = Rename get
  set = Rename . set

instance ReaderM Rename RO where
  ask = Rename ask

instance RunReaderM Rename RO where
  local ro = Rename . local ro . unRename

subst :: Var -> Rename Var
subst v = do
  ro <- ask
  case Map.lookup v (roSubst ro) of
    Nothing -> return v
    Just x  -> return x

fresh :: [Var] -> Rename a -> Rename a
fresh vs m = do
  i  <- get
  ro <- ask
  let (ro',i') = findFresh ro i vs
  set i'
  local ro' m

findFresh :: RO -> Int -> [Var] -> (RO,Int)
findFresh  = loop
  where
  loop ro i []         = (ro, i)
  loop ro i (v:vs)
    | ro `captures` v' = loop ro                 i' (v:vs)
    | otherwise        = loop (addSubst v v' ro) i'    vs
    where
    i' = i + 1
    v' = "_rename_" ++ v ++ show i



-- Term Renaming ---------------------------------------------------------------

renameTopDecls :: [Decl] -> [Decl]
renameTopDecls ds = runRename bound (renameDecls ds)
  where
  bound = map declName ds

-- | Rename declarations, assuming that names are already present in the
-- environment.
renameDecls :: [Decl] -> Rename [Decl]
renameDecls  = mapM renameDecl

-- | Rename a declaration, assuming a fresh name is already in the environment.
renameDecl :: Decl -> Rename Decl
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
renameTerm :: Term -> Rename Term
renameTerm t =
  case t of
    Abs vs b -> fresh vs (Abs <$> mapM subst vs <*> renameTerm b)
    App f x  -> App <$> renameTerm f <*> renameTerm x
    Var v    -> Var <$> subst v
    Lit l    -> Lit <$> renameLiteral l
    Let ds e ->
      fresh (map declName ds) (Let <$> mapM renameDecl ds <*> renameTerm e)

-- | Rename literals.  For the time being, this doesn't do anything.
renameLiteral :: Literal -> Rename Literal
renameLiteral (LInt i) = return (LInt i)
