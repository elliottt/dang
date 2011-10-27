{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}


module Compile.LambdaLift (
    lambdaLift
  ) where

import Dang.IO
import Dang.Monad
import Core.AST
import Pretty (pretty)
import QualName (QualName)
import TypeChecker.Types
import TypeChecker.Unify (Types(typeVars),quantify,Instantiate,inst')
import Variables
    (sccFreeQualNames,sccToList,FreeVars(),freeLocals)

import Control.Applicative (Applicative,(<$>))
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set


lambdaLift :: Module -> Dang Module
lambdaLift m = do
  logStage "lambda-lifter"
  m' <- runLL (llModule m)
  logInfo "lambda-lifter output"
  logDebug (show m')
  logInfo (pretty m')
  return m'


-- Lambda-lifting Monad --------------------------------------------------------

newtype LL a = LL
  { unLL :: StateT Rename (ReaderT Env (WriterT [Decl] Dang)) a
  } deriving (Functor,Applicative,Monad)

instance ExceptionM LL SomeException where
  raise = LL . raise

instance BaseM LL Dang where
  inBase = LL . inBase

instance WriterM LL [Decl] where
  put = LL . put

instance RunWriterM LL [Decl] where
  collect m = LL (collect (unLL m))

instance ReaderM LL Env where
  ask = LL ask

instance RunReaderM LL Env where
  local env m = LL (local env (unLL m))

runLL :: LL a -> Dang a
runLL (LL m) =
  f `fmap` runWriterT (runReaderT emptyEnv (runStateT emptyRename m))
  where
  f = fst . fst


-- Renaming and Lifting --------------------------------------------------------

type Rename = Map.Map QualName Term

emptyRename :: Rename
emptyRename  = Map.empty

-- | Produce either a reference to a global variable, or the term that it is to
-- be replaced with.
subst :: QualName -> LL Term
subst qn = LL $ do
  u <- get
  case Map.lookup qn u of
    Just tm -> return tm
    Nothing -> return (Global qn)

-- | Extend the closure, and return a new call-pattern for the symbol.
addClosure :: [(Var,Type)] -> Decl -> (Decl,Term)
addClosure clos = extend
  where
  tyVars    = Set.toList (typeVars (map snd clos))
  addVars   = foldl (\f (v,ty) -> MPat (PVar v ty) . f) id clos
  extend d  = (d',call)
    where
    d'   = d { declBody = quantify tyVars (addVars (forallData (declBody d))) }
    call = app (appT (Global (declName d)) (map TVar tyVars))
         $ map (Local . fst) clos

typedClosure :: (FreeVars a) => a -> LL [(Var,Type)]
typedClosure  = mapM step . Set.toList . freeLocals
  where
  step a = (a,) <$> typeOfLocal a

rename :: QualName -> Term -> LL ()
rename qn tm = LL $ do
  u <- get
  set $! Map.insert qn tm u


-- Type Environment ------------------------------------------------------------

data Env = Env
  { envParams :: Set.Set TParam
  , envTypes  :: Map.Map Var Type
  } deriving (Show)

emptyEnv :: Env
emptyEnv  = Env
  { envParams = Set.empty
  , envTypes  = Map.empty
  }

addLocal :: Var -> Type -> Env -> Env
addLocal n ty env = env { envTypes = Map.insert n ty (envTypes env) }

addParams :: Set.Set TParam -> Env -> Env
addParams ps env = env { envParams = ps `Set.union` envParams env }

introVars :: [TParam] -> LL a -> LL a
introVars ps k = do
  env <- ask
  local (addParams (Set.fromList ps) env) k

introPat :: Pat -> LL a -> LL a
introPat p k = case p of

  PVar v ty -> do
    env <- ask
    local (addLocal v ty env) k

  PWildcard _ -> k

typeOfLocal :: Var -> LL Type
typeOfLocal v = do
  env <- ask
  case Map.lookup v (envTypes env) of
    Just ty -> return ty
    Nothing -> fail "typeOfLocal"


-- Core Lambda-lifting ---------------------------------------------------------

llModule :: Module -> LL Module
llModule m = do
  (ds',ls) <- collect (mapM llTopDecl (modDecls m))
  return m { modDecls = ds' ++ ls }

llTopDecl :: Decl -> LL Decl
llTopDecl d = do
  logInfo ("processing " ++ pretty (declName d))
  b' <- llForall llMatch (declBody d)
  return d { declBody = b' }

llForall :: (Show a, Types a, Instantiate a) => (a -> LL a) -> Forall a -> LL (Forall a)
llForall k (Forall ps a) = introVars ps $ do
  a' <- k (inst' (map TVar ps) a)
  return (quantify ps a')

llMatch :: Match -> LL Match
llMatch m = case m of

  MPat p b -> MPat p <$> introPat p (llMatch b)

  MTerm tm ty -> do
    tm' <- llTerm tm
    return (MTerm tm' ty)

-- | Process a series of declarations from a let-expression, lifting out any
-- that contain arguments, and generating appropriate substitutions.
llLetDecls :: [Decl] -> LL [Decl]
llLetDecls  = foldM step [] . sccFreeQualNames
  where
  step acc ds = do
    ds' <- llLetBlock (sccToList ds)
    return (acc ++ ds')

{-
llLetDecl :: Decl -> LL [Decl]
llLetDecl d = do
  -- generate the replacement term for this declaration
  clos <- typedClosure d
  let (d',call) = addClosure clos d
  rename (declName d) call
  b' <- llForall llMatch (declBody d')
  let decl = d' { declBody = b' }
  if hasArgs decl
     then put [decl] >> return []
     else return [decl]
     -}

llLetBlock :: [Decl] -> LL [Decl]
llLetBlock ds = do
  clos <- typedClosure ds

  let extend d = do
        let (d',call) = addClosure clos d
        rename (declName d') call
        return d'

      step d = do
        b' <- llForall llMatch (declBody d)
        let decl = d { declBody = b' }
        if hasArgs decl
           then put [decl] >> return []
           else return [decl]

  fmap concat . mapM step =<< mapM extend ds

llTerm :: Term -> LL Term
llTerm tm = case tm of

  AppT f ts -> do
    f' <- llTerm f
    return (appT f' ts)

  App f xs -> do
    f'  <- llTerm f
    xs' <- mapM llTerm xs
    return (app f' xs')

  Let ds e -> do
    ds' <- llLetDecls ds
    e'  <- llTerm e
    return (letIn ds' e')

  Global qn -> subst qn

  Local _ -> return tm
  Lit _   -> return tm
