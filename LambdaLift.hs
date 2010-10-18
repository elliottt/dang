{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LambdaLift where

import AST
import Pretty
import Rename

import Control.Applicative (Applicative(..),(<$>))
import Control.Arrow (first)
import Data.Graph (SCC(..))
import Data.List (partition)
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set


type Subst = Map.Map Var Term

newtype LL a = LL
  { unLL :: StateT Subst (WriterT [Decl] Lift) a
  } deriving (Functor,Applicative,Monad)

runLL :: LL a -> (a,[Decl])
runLL  = first fst . runLift . runWriterT . runStateT Map.empty . unLL

instance WriterM LL [Decl] where
  put = LL . put

instance StateM LL Subst where
  get = LL get
  set = LL . set

emit :: Decl -> LL ()
emit d = emits [d]

emits :: [Decl] -> LL ()
emits  = put . map notExported

extend :: [(Var,Term)] -> LL ()
extend ns = do
  u <- get
  set $! Map.union (Map.fromList ns) u

subst :: Var -> LL Term
subst v = do
  u <- get
  case Map.lookup v u of
    Nothing -> return (Var v)
    Just t  -> return t

lambdaLift :: [Decl] -> [Decl]
lambdaLift ds = uncurry (++) (runLL (llDecls ds))

extendVars :: [Var] -> Decl -> Decl
extendVars vs d = d { declVars = vs ++ declVars d }

llDecls :: [Decl] -> LL [Decl]
llDecls  = fmap concat . mapM step . sccDecls
  where
  step (AcyclicSCC d) = do
    let n   = declName d
    let fvs = Set.toList (Set.delete n (freeVars d))
    extend [ (n, lambda fvs (Var n)) ]
    d' <- llDecl d
    return [extendVars fvs d']
  step (CyclicSCC ds) = do
    let ns  = declNames ds
    let fvs = Set.toList (freeVars ds Set.\\ Set.fromList ns)
    extend [ (n, lambda fvs (Var n)) | n <- ns ]
    mapM (fmap (extendVars fvs) . llDecl) ds

llDecl :: Decl -> LL Decl
llDecl d = do
  b' <- llTerm (declBody d)
  return d { declBody = b' }

llTerm :: Term -> LL Term
llTerm t =
  case t of
    Abs{}    -> error "Abs should have been removed during renaming"
    App f x  -> App <$> llTerm f <*> llTerm x
    Var v    -> subst v
    Lit l    -> return (Lit l)
    Let ds e -> do
      ds' <- llDecls ds
      let (as,bs) = partition hasArguments ds'
      emits as
      e' <- llTerm e
      if null bs
         then return e'
         else return (Let bs e')
