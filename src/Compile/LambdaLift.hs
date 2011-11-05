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
import TypeChecker.Unify (Types(typeVars),quantify,quantifyAux,inst)
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

-- | Extend the closure, and return a new call-pattern for the symbol.  Add the
-- type variables from the free variables, and the free variables to the
-- arguments.
addClosure :: [(Var,Type)] -> Decl -> (Decl,Call)
addClosure clos = extend
  where
  tyVars    = Set.toList (typeVars (map snd clos))
  addVars   = foldl (\f (v,ty) -> MPat (PVar v ty) . f) id clos
  extend d  = (d',call)
    where
    d'      = d { declBody = Forall (forallParams body ++ qs) b' }
    (qs,b') = quantifyAux off tyVars (addVars (forallData body))
    body    = declBody d
    off     = length (forallParams body)
    call    = (map uvar tyVars, map (Local . fst) clos)

typedClosure :: (FreeVars a) => a -> LL [(Var,Type)]
typedClosure  = mapM step . Set.toList . freeLocals
  where
  step a = (a,) <$> typeOfLocal a

rename :: QualName -> Call -> LL ()
rename qn call = LL $ do
  u <- get
  set $! Map.insert qn call u


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

llForall :: (Pretty a, Types a) => (a -> LL a) -> Forall a -> LL (Forall a)
llForall k (Forall ps a) = introVars ps $ \ ps' -> do
  let i = inst (map uvar ps') a
  a' <- k i
  return (quantify ps' a')

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

llLetBlock :: [Decl] -> LL [Decl]
llLetBlock ds = do
  logInfo ("processing " ++ pretty (map declName ds))

  clos <- typedClosure ds
  logInfo ("  closure: " ++ pretty clos)

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

  AppT g@(Global qn) ts -> do
    (ts',vs) <- subst qn
    return (app (appT g (ts ++ ts')) vs)

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

  Global qn -> do
    (ts,vs) <- subst qn
    return (app (appT tm ts) vs)

  Local _ -> return tm
  Lit _   -> return tm
