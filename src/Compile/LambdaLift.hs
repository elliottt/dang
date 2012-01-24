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
import Pretty (Pretty,pretty)
import QualName (QualName)
import TypeChecker.Types
import TypeChecker.Unify (Types(genVars))
import Variables (sccFreeQualNames,sccToList,FreeVars(),freeLocals)

import Control.Applicative (Applicative,(<$>),(<*>))
import Data.List (partition,(\\))
import MonadLib hiding (lift)
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

type Call = ([Type],[Term])

type Rename = Map.Map QualName Call

emptyRename :: Rename
emptyRename  = Map.empty

-- | Produce either a reference to a global variable, or the term that it is to
-- be replaced with.
subst :: QualName -> LL Call
subst qn = LL $ do
  u <- get
  case Map.lookup qn u of
    Just tm -> return tm
    Nothing -> return ([],[])

type Closure = [(Var,Type)]

emptyClosure :: Closure
emptyClosure  = []

closureArgs :: Closure -> Match -> Match
closureArgs  = loop
  where
  loop clos = case clos of
    (v,ty):cs -> MPat (PVar v ty) . loop cs
    []        -> id

typedClosure :: (FreeVars a) => a -> LL Closure
typedClosure  = mapM step . Set.toList . freeLocals
  where
  step a = (a,) <$> typeOfLocal a

closureParams :: Closure -> [TParam]
closureParams  = Set.toList . genVars . map snd

closureTerms :: Closure -> [Term]
closureTerms clos = [ Local v | (v,_) <- clos ]

rewriteSyms :: [(QualName,Call)] -> LL ()
rewriteSyms rs = LL $ do
  u <- get
  set $! Map.fromList rs `Map.union` u


-- Type Environment ------------------------------------------------------------

data Env = Env
  { envParams :: Set.Set TParam
  , envTypes  :: Map.Map Var Type
  , envDepth  :: !Int
  } deriving (Show)

emptyEnv :: Env
emptyEnv  = Env
  { envParams = Set.empty
  , envTypes  = Map.empty
  , envDepth  = 0
  }

addLocal :: Var -> Type -> Env -> Env
addLocal n ty env = env { envTypes = Map.insert n ty (envTypes env) }

-- | Extend the environment with information about type variables to be
-- introduced.
addParams :: Set.Set TParam -> Env -> Env
addParams ps env = env
  { envParams = ps `Set.union` envParams env
  , envDepth  = envDepth env + Set.size ps
  }

-- | Introduce type variables, adding them to the environment as captured.
introVars :: [TParam] -> ([TParam] -> LL a) -> LL a
introVars ps k = do
  env <- ask
  let ps' = [ p { paramIndex = envDepth env + paramIndex p } | p <- ps ]
  local (addParams (Set.fromList ps') env) (k ps')

introPat :: Pat -> LL a -> LL a
introPat p k = do
  env   <- ask
  local (addPatLocals p env) k

addPatLocals :: Pat -> Env -> Env
addPatLocals p = case p of
  PVar v ty   -> addLocal v ty
  PCon _ ps _ -> foldl (\k p' -> k . addPatLocals p') id ps
  PWildcard _ -> id

typeOfLocal :: Var -> LL Type
typeOfLocal v = do
  env <- ask
  case Map.lookup v (envTypes env) of
    Just ty -> return ty
    Nothing -> fail ("typeOfLocal: " ++ v)


-- Core Lambda-lifting ---------------------------------------------------------

llModule :: Module -> LL Module
llModule m = do
  (ds',ls) <- collect (mapM (llDecl [] emptyClosure) (modDecls m))
  return m { modDecls = ds' ++ ls }

llDecl :: [TParam] -> Closure -> Decl -> LL Decl
llDecl ps clos d = do
  logInfo ("processing " ++ pretty (declName d))
  b' <- llForall llMatch (declBody d)
  return $ extendClosure ps (closureArgs clos)
         $ d { declBody = b' }

llForall :: (Pretty a, Types a) => (a -> LL a) -> Forall a -> LL (Forall a)
llForall k (Forall ps a) = introVars ps $ \ _ps' -> do
  a' <- k a
  return (Forall ps a')

llMatch :: Match -> LL Match
llMatch m = case m of

  MPat p b -> MPat p <$> introPat p (llMatch b)

  MSplit l r -> MSplit <$> llMatch l <*> llMatch r

  MTerm tm ty -> do
    tm' <- llTerm tm
    return (MTerm tm' ty)

  MFail _ -> return m

-- | Process a series of declarations from a let-expression, lifting out any
-- that contain arguments, and generating appropriate substitutions.
llLetDecls :: [Decl] -> LL [Decl]
llLetDecls  = foldM step [] . sccFreeQualNames
  where
  step acc ds = do
    ds' <- llLetBlock (sccToList ds)
    return (acc ++ ds')

-- | Process a group of mutually recursive let-declarations.
llLetBlock :: [Decl] -> LL [Decl]
llLetBlock ds = do
  let ns = map declName ds
  logInfo ("processing group: " ++ pretty ns)

  clos <- typedClosure ds
  logInfo ("  closure: " ++ pretty clos)

  let newParams = closureParams clos
      call      = (map gvar newParams, closureTerms clos)
  rewriteSyms (zip ns (repeat call))

  ds' <- mapM (llDecl newParams clos) ds

  let (lift,keep) = partition hasArgs ds'
  put lift
  return keep

llTerm :: Term -> LL Term
llTerm tm = case tm of

  -- catch type application to extend with any new arguments
  AppT g@(Global qn) ts -> do
    (ts',vs) <- subst qn
    let extra = ts' \\ ts
    return (app (appT g (extra ++ ts)) vs)

  AppT f ts -> do
    f' <- llTerm f
    return (appT f' ts)

  App f xs -> do
    f'  <- llTerm f
    xs' <- mapM llTerm xs
    return (app f' xs')

  Case e m -> do
    e' <- llTerm e
    m' <- llMatch m
    return (Case e' m')

  Let ds e -> do
    ds' <- llLetDecls ds
    e'  <- llTerm e
    return (letIn ds' e')

  Global qn -> do
    (ts,vs) <- subst qn
    return (app (appT tm ts) vs)

  Local _ -> return tm
  Lit _   -> return tm
