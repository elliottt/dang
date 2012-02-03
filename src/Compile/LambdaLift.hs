{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}


module Compile.LambdaLift (
    lambdaLift
  ) where

import Core.AST
import Dang.IO
import Dang.Monad
import Pretty (pretty)
import QualName (QualName,Name,simpleName,qualSymbol,isSimpleName)
import TypeChecker.Types (Type,TParam,uvar,Forall(..),modifyTParamIndex)
import TypeChecker.Unify (inst,quantify,typeVars)
import Variables (freeLocals)

import Control.Applicative(Applicative(..),(<$>))
import Data.List (partition)
import Data.Maybe (isJust)
import MonadLib hiding (lift)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- External Interface ----------------------------------------------------------

lambdaLift :: Module -> Dang Module
lambdaLift m = do
  logStage "lambda-lifter"
  m' <- runLL (llModule m)
  logInfo "lambda-lifter output:"
  logInfo (pretty m')
  logDebug (show m')
  return m'


-- Lambda-lifting Monad --------------------------------------------------------

newtype LL a = LL
  { unLL :: ReaderT Env (WriterT [Decl] Dang) a
  } deriving (Functor,Applicative,Monad)

instance BaseM LL Dang where
  inBase = LL . inBase

instance WriterM LL [Decl] where
  put = LL . put

instance RunWriterM LL [Decl] where
  collect = LL .collect . unLL

instance ReaderM LL Env where
  ask = LL ask

instance RunReaderM LL Env where
  local env m = LL (local env (unLL m))

runLL :: LL a -> Dang a
runLL m = do
  (a,_) <- runWriterT (runReaderT emptyEnv (unLL m))
  return a


-- Lambda-lifting Environment --------------------------------------------------

data Env = Env
  { envDepth   :: !Int                 -- ^ Quantifier depth
  , envArgs    :: Map.Map Name Type    -- ^ Arguments
  , envRewrite :: Map.Map Name Rewrite -- ^ Call-rewriting
  } deriving Show

emptyEnv :: Env
emptyEnv  = Env
  { envDepth   = 0
  , envArgs    = Map.empty
  , envRewrite = Map.empty
  }

addArg :: Name -> Type -> Env -> Env
addArg n ty env = env { envArgs = Map.insert n ty (envArgs env) }

addRewrite :: Name -> Rewrite -> Env -> Env
addRewrite n rw env = env { envRewrite = Map.insert n rw (envRewrite env) }

introParams :: [TParam] -> ([TParam] -> LL a) -> LL a
introParams ps k = do
  env <- ask
  let ps' = map (modifyTParamIndex (+ envDepth env)) ps
  local (env { envDepth = envDepth env + length ps }) (k ps')

introPatArgs :: Pat -> LL a -> LL a
introPatArgs p k = do
  env <- ask
  local (patArgs p env) k

patArgs :: Pat -> Env -> Env
patArgs p env = case p of
  PVar n ty   -> addArg n ty env
  PCon _ ps _ -> foldr patArgs env ps
  PWildcard _ -> env

lookupType :: Name -> LL Type
lookupType n = do
  env <- ask
  case Map.lookup n (envArgs env) of
    Just ty -> return ty
    Nothing -> fail ("lookupType: " ++ n)

lookupRewrite :: Name -> LL Rewrite
lookupRewrite n = do
  env <- ask
  case Map.lookup n (envRewrite env) of
    Just rw -> return rw
    Nothing -> return emptyRewrite


-- Core Lambda-lifting ---------------------------------------------------------

llModule :: Module -> LL Module
llModule m = do
  (ds,ls) <- collect (mapM llDecl (modDecls m))
  return m { modDecls = ds ++ ls }

llDecl :: Decl -> LL Decl
llDecl d = do
  logInfo ("processing " ++ pretty (declName d))
  d' <- llBody (qualSymbol (declName d)) (declBody d)
  return d { declBody = d' }

llBody :: Name -> Forall Match -> LL (Forall Match)
llBody n qb = introParams (forallParams qb) $ \ ps -> do
  let vars = map uvar ps
  rw    <- lookupRewrite n
  body' <- addRewriteArgs rw =<< llMatch (inst vars (forallData qb))
  return (quantify (ps ++ rewriteParams rw) body')

llMatch :: Match -> LL Match
llMatch m = case m of

  MPat p m' -> introPatArgs p (MPat p <$> llMatch m')

  MSplit l r -> MSplit <$> llMatch l <*> llMatch r

  MTerm tm ty -> MTerm <$> llTerm tm <*> pure ty

  MFail _  -> return m

llTerm :: Term -> LL Term
llTerm tm = case tm of

  AppT (Local n) ps -> llLocal n ps []
  App  (Local n) xs -> llLocal n [] xs
  Local n           -> llLocal n [] []

  AppT (Global qn) ps -> llGlobal qn ps []
  App  (Global qn) xs -> llGlobal qn [] xs
  Global qn           -> llGlobal qn [] []

  AppT f tys -> AppT <$> llTerm f <*> pure tys

  App f xs -> App <$> llTerm f <*> mapM llTerm xs

  Case e m -> Case <$> llTerm e <*> llMatch m

  Let ds e -> llLet ds e

  Lit _    -> return tm

llLocal :: Name -> [Type] -> [Term] -> LL Term
llLocal n ps xs = do
  rw <- lookupRewrite n
  let ps' = ps ++ rewriteTypes rw
      sym | nullRewrite rw = Local n
          | otherwise      = Global (simpleName n)
  return (app (appT sym ps') (rewriteTerms rw ++ xs))

-- this should really be not possible, as the module system can tell when
-- something is locally defined.
llGlobal :: QualName -> [Type] -> [Term] -> LL Term
llGlobal qn ps xs
  | isSimpleName qn = llLocal (qualSymbol qn) ps xs
  | otherwise       = return (app (appT (Global qn) ps) xs)


-- Let-expressions -------------------------------------------------------------

-- | The set of escaping type and local variables in a declaration.
data EscapeSet = EscapeSet
  { escParams :: Set.Set TParam
  , escArgs   :: Set.Set Name
  , escLocals :: Set.Set Name
  , escSimple :: Maybe Type
  } deriving Show

type EscapeSets = Map.Map Name EscapeSet

simpleType :: Decl -> Maybe Type
simpleType d = case declType d of
  Forall [] ty -> Just ty
  _            -> Nothing

escapeSet :: Decl -> LL EscapeSet
escapeSet d = do
  env <- ask
  let locals  = freeLocals (declBody d)
      inScope = Set.fromList (Map.keys (envArgs env))
      args    = locals `Set.intersection` inScope
  return EscapeSet
    { escParams = typeVars (declBody d)
    , escArgs   = args
    , escLocals = locals Set.\\ args
    , escSimple = simpleType d
    }

escapeSets :: [Decl] -> LL EscapeSets
escapeSets  = foldM step Map.empty
  where
  step exs d = do
    ex <- escapeSet d
    return $! Map.insert (qualSymbol (declName d)) ex exs

-- | Solve the local escapes in each @EscapeSet@.
solveEscapeSets :: EscapeSets -> EscapeSets
solveEscapeSets exs0 = foldl step exs0 (Map.keys exs0)
  where
  step exs qn = Map.adjust (solveEscapeSet exs) qn exs

solveEscapeSet :: EscapeSets -> EscapeSet -> EscapeSet
solveEscapeSet exs ex = ex
  { escParams = sps
  , escArgs   = sargs
  , escLocals = Set.empty
  }
  where
  (sps,sargs) = solve Set.empty (escParams ex) (escArgs ex)
      (Set.toList (escLocals ex))

  solve solved ps args locals = case locals of

    l:ls -> case Map.lookup l exs of

      Just lx ->
        let solved' = Set.insert l solved
            ps'     = Set.union ps (escParams lx)
            k a e   = solve solved' ps' (Set.union args a) (e ++ ls)
         in if isJust (escSimple lx)
               -- escape set of a definition: reference the name
               then k (Set.singleton l) []
               -- escape set of a function: fully solve
               else k (escArgs lx) (Set.toList (escLocals lx Set.\\ solved'))

      Nothing -> solve solved ps args ls

    [] -> (ps,args)

-- | Additions to the call pattern, and closure, of a declaration.
data Rewrite = Rewrite
  { rewriteParams :: [TParam]
  , rewriteArgs   :: [Name]
  } deriving Show

emptyRewrite :: Rewrite
emptyRewrite  = Rewrite
  { rewriteParams = []
  , rewriteArgs   = []
  }

rewriteTypes :: Rewrite -> [Type]
rewriteTypes  = map uvar . rewriteParams

rewriteTerms :: Rewrite -> [Term]
rewriteTerms  = map Local . rewriteArgs

nullRewrite :: Rewrite -> Bool
nullRewrite rw = null (rewriteParams rw) && null (rewriteArgs rw)

escapeSetRewrite :: EscapeSet -> Rewrite
escapeSetRewrite ex = Rewrite
  { rewriteParams = Set.toList (escParams ex)
  , rewriteArgs   = Set.toList (escArgs ex)
  }

addRewriteArgs :: Rewrite -> Match -> LL Match
addRewriteArgs rw m0 = foldM step m0 (reverse (rewriteArgs rw))
  where
  step m n = do
    ty <- lookupType n
    return (MPat (PVar n ty) m)

escapeSetsEnv :: EscapeSets -> Env -> Env
escapeSetsEnv exs env0 = Map.foldrWithKey step env0 exs
  where
  step n ex env = case escSimple ex of
    Just ty -> addArg n ty env
    Nothing -> addRewrite n (escapeSetRewrite ex) env

llLet :: [Decl] -> Term -> LL Term
llLet ds e = do
  exs <- escapeSets ds
  let solved = solveEscapeSets exs
  env <- ask
  local (escapeSetsEnv solved env) $ do
    ds' <- mapM llDecl ds
    let (lift,keep) = partition hasArgs ds'
    put lift
    letIn keep <$> llTerm e
