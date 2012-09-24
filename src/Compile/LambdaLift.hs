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
import QualName (QualName,Name,simpleName,qualSymbol)
import TypeChecker.Types (Type,uvar,Forall(..),Kind)
import TypeChecker.Unify (inst,quantify,typeVars)
import TypeChecker.Vars (TParam,modifyTParamIndex)
import Variables (freeLocals,freeVars)

import Control.Applicative(Applicative(..),(<$>))
import Data.Generics (extM,Data(gmapM))
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
  { envDepth   :: !Int              -- ^ Quantifier depth
  , envArgs    :: Map.Map Name Type -- ^ Arguments
  , envRewrite :: RewriteMap        -- ^ Call-rewriting
  } deriving Show

emptyEnv :: Env
emptyEnv  = Env
  { envDepth   = 0
  , envArgs    = Map.empty
  , envRewrite = Map.empty
  }

addArg :: Name -> Type -> Env -> Env
addArg n ty env = env { envArgs = Map.insert n ty (envArgs env) }

addRewrites :: RewriteMap -> Env -> Env
addRewrites rws env = env { envRewrite = Map.union rws (envRewrite env) }

introParams :: [TParam Kind] -> ([TParam Kind] -> LL a) -> LL a
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

lookupRewrite :: QualName -> LL (Maybe Rewrite)
lookupRewrite qn = do
  env <- ask
  return $! Map.lookup qn (envRewrite env)


-- Core Lambda-lifting ---------------------------------------------------------

llPass :: Data a => a -> LL a
llPass  =
  gmapM llPass
  `extM` llDecl
  `extM` llMatch
  `extM` llTerm

llModule :: Module -> LL Module
llModule m = do
  (ds,ls) <- collect (llPass (modDecls m))
  return m { modDecls = ds ++ ls }

llDecl :: Decl -> LL Decl
llDecl d = do
  logInfo ("processing " ++ pretty (declName d))
  d' <- llBody (declName d) (declBody d)
  return d { declBody = d' }

llBody :: QualName -> Forall Match -> LL (Forall Match)
llBody qn qb = introParams (forallParams qb) $ \ ps -> do
  let vars = map uvar ps
  mb    <- lookupRewrite qn
  let (extraParams,extraArgs) = case mb of
        Just rw -> (rewriteParams rw, addRewriteArgs rw)
        Nothing -> ([],return)
  body' <- extraArgs =<< llMatch (inst vars (forallData qb))
  return (quantify (ps ++ extraParams) body')

llMatch :: Match -> LL Match
llMatch m = case m of
  MPat p m' -> introPatArgs p (MPat p <$> llMatch m')
  _         -> gmapM llPass m

llTerm :: Term -> LL Term
llTerm tm = case tm of

  -- let-bound names are local, rewritten to global if they get lifted
  AppT (Local n) ps -> llLocal n ps
  Local n           -> llLocal n []

  -- lift let-bound declarations
  Let ds e -> llLet ds e

  _ -> gmapM llPass tm

llLocal :: Name -> [Type] -> LL Term
llLocal n ps = maybe (Local n) rewriteSymbol <$> lookupRewrite qn
  where
  qn               = simpleName n
  rewriteSymbol rw = app (appT sym ps') (rewriteTerms rw)
    where
    ps' = ps ++ rewriteTypes rw
    sym = Global qn

llLet :: [Decl] -> Term -> LL Term
llLet ds e = do
  env <- ask
  let args = Set.fromList (Map.keys (envArgs env))
  rewrites <- buildRewriteMap args ds
  local (addRewrites rewrites env) $ do
    ds' <- mapM llDecl ds
    let (lift,keep) = partition hasArgs ds'
    put lift
    letIn keep <$> llTerm e


-- Rewrite Context -------------------------------------------------------------

type RewriteMap = Map.Map QualName Rewrite

-- | Given a list of declarations, generate the rewrite map that satisfies their
-- closures.
buildRewriteMap :: Args -> [Decl] -> LL RewriteMap
buildRewriteMap args ds = solve `fmap` escapeSets args (peerSet ds) ds
  where
  solve = escapeSetsRewriteMap . solveEscapeSets

-- | A rewrite represents the extra information needed by a closure.
data Rewrite = Rewrite
  { rewriteParams :: [TParam Kind]
  , rewriteArgs   :: [Name]
  , rewritePeers  :: [Name]
  } deriving Show

-- | The list of types needed by the closure.
rewriteTypes :: Rewrite -> [Type]
rewriteTypes rw = map uvar (rewriteParams rw)

-- | The list of environmental arguments needed by the closure.
rewriteTerms :: Rewrite -> [Term]
rewriteTerms rw = locals ++ globals
  where
  locals  = map Local (rewriteArgs rw)
  globals = map (Global . simpleName) (rewritePeers rw)

-- | Rewrite a declaration ot include the closure arguments.
addRewriteArgs :: Rewrite -> Match -> LL Match
addRewriteArgs rw body = foldM step body (reverse args)
  where
  args     = rewriteArgs rw ++ rewritePeers rw
  step m n = do
    ty <- lookupType n
    return (MPat (PVar n  ty) m)


-- Escape Sets -----------------------------------------------------------------

-- | Escape sets are a collection of individual escape sets, organized by their
-- name.
type EscapeSets = Map.Map QualName EscapeSet

-- | Calculate escape sets for a group of declarations.
escapeSets :: Args -> Peers -> [Decl] -> LL EscapeSets
escapeSets args peers = foldM step Map.empty
  where
  step exs d = do
    ex <- escapeSet args peers d
    return $! Map.insert (declName d) ex exs

-- | Generate a @RewriteMap@ given a (solved) @EscapeSets@.  This will not
-- generate a @RewriteMap@ entry for @EscapeSet@s that describe simple
-- declarations.
escapeSetsRewriteMap :: EscapeSets -> RewriteMap
escapeSetsRewriteMap  = Map.foldlWithKey step Map.empty
  where
  step rws qn ex
    | isJust (escSimple ex) = rws
    | otherwise             = Map.insert qn (escapeSetRewrite ex) rws

-- | Given a map of @EscapeSets@, iterate over each entry, calculating its
-- transitive closure.
solveEscapeSets :: EscapeSets -> EscapeSets
solveEscapeSets exs0 = foldl step exs0 (Map.keys exs0)
  where
  step exs qn = Map.adjust (solveEscapeSet exs) qn exs

-- | Escape sets represent the list of type parameters, arguments and local
-- declarations that show up free in a declaration body.
data EscapeSet = EscapeSet
  { escParams :: Set.Set (TParam Kind)
  , escArgs   :: Set.Set Name
  , escPeers  :: Set.Set Name
  , escSimple :: Maybe Type
  } deriving Show

-- | Generate a @Rewrite@ given the context of an @EscapeSet@.
escapeSetRewrite :: EscapeSet -> Rewrite
escapeSetRewrite esc = Rewrite
  { rewriteParams = Set.toList (escParams esc)
  , rewriteArgs   = Set.toList (escArgs esc)
  , rewritePeers  = Set.toList (escPeers esc)
  }

-- | Calculate the transitive closure for an escape set, given the other
-- @EscapeSet@s in its binding group.
solveEscapeSet :: EscapeSets -> EscapeSet -> EscapeSet
solveEscapeSet exs ex
  = solve Set.empty (escParams ex) (escArgs ex) Set.empty
  $ Set.toList
  $ escPeers ex
  where
  solve solved ps args peers goals = case goals of

    g:gs -> case Map.lookup (simpleName g) exs of

      Just gx ->
        let solved'  = Set.insert g solved
            ps'      = Set.union ps (escParams gx)
            unsolved = Set.toList (escPeers gx Set.\\ solved')
            k        = solve solved' ps'
         in if isJust (escSimple gx)
               -- escape set of a definition: reference the name
               then k args (Set.insert g peers) gs
               -- escape set of a function: fully solve
               else k (args `Set.union` escArgs gx) peers (unsolved ++ gs)

      Nothing -> solve solved ps args peers gs

    [] -> ex
      { escParams = ps
      , escArgs   = args
      , escPeers  = peers
      }

-- | A set of arguments required by a declaration.
type Args = Set.Set Name

-- | A set of other named declarations used by another declaration.
type Peers = Set.Set QualName

-- | Compute the peer set for a list of declarations.  This will only work for
-- declarations that do not have fully qualified names, as it discards all
-- namespace information.
peerSet :: [Decl] -> Peers
peerSet  = Set.fromList . map declName

-- | Calculate the escape set for a single declaration.
escapeSet :: Args -> Peers -> Decl -> LL EscapeSet
escapeSet args peers d = do
  let locals = freeLocals d `Set.intersection` args
  tys <- mapM lookupType (Set.toList locals)
  let localParams = typeVars tys
  return EscapeSet
    { escParams = typeVars (declBody d) `Set.union` localParams
    , escArgs   = locals
    , escPeers  = Set.map qualSymbol (freeVars d `Set.intersection` peers)
    , escSimple = simpleType d
    }

-- | Return the type of a declaration, if it is monomorphic and contains no
-- arguments.
simpleType :: Decl -> Maybe Type
simpleType d = do
  guard (not (hasArgs d))
  let Forall ps ty = declType d
  guard (null ps)
  return ty
