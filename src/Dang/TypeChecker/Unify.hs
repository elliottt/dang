{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Trustworthy #-}

module Dang.TypeChecker.Unify where

import Dang.TypeChecker.Types
import Dang.Utils.Pretty

import qualified Data.Map as Map
import           Data.Monoid ( Monoid(..) )
import qualified Data.Set as Set
import           MonadLib
                    ( runM, Id, StateT, get, set, ReaderT, ask, ExceptionT
                    , raise )


-- | A failure during unification.
data UnifyError = UnifyError Type Type
                | MatchError Type Type
                  deriving (Show,Eq)

instance Pretty UnifyError where

  ppr (UnifyError x y) =
    hang (fsep [ text "couldn't unify", quoted (pp x)
               , text "with", quoted (pp y) ])
       2 (vcat [ text "Expected type:" <+> pp x
               , text "  Actual type:" <+> pp y ])

  ppr (MatchError x y) =
    hang (fsep [ text "couldn't match", quoted (pp x)
               , text "with", quoted (pp y) ])
       2 (vcat [ text "Expected type:" <+> pp x
               , text "  Actual type:" <+> pp y ])


type MGU = StateT VarEnv (ExceptionT UnifyError Id)

-- | Bind a variable to a type.
varBind :: TParam -> Type -> MGU (Maybe Type)
varBind p ty =
  do u <- get
     case lookupFree p u of

       -- the variable hasn't been bound, add an entry in the map
       Nothing -> do set (bindFree p ty `mappend` u)
                     return Nothing

       -- the variable is already bound, continue with unfication
       res -> return res


-- MGU Calculation -------------------------------------------------------------

-- | Compute the most-general unifier for two types, given a variable
-- environment.  Produces either an error, or an extended substitution.
mgu :: Type -> Type -> VarEnv -> Either UnifyError VarEnv
mgu t1 t2 env = case runM (go t1 t2) env of
                  Right (_,env') -> Right env'
                  Left err       -> Left err
  where
  go :: Type -> Type -> MGU ()
  go (TApp f x) (TApp g y) =
    do go f g
       go x y

  -- XXX should these be annotated with kinds?
  go (TCon n1) (TCon n2)
    | n1 == n2 = return ()

  -- resolve or bind variables
  go (TVar p) b = bind p b
  go a (TVar p) = bind p a

  -- bound variables only unify with themselves
  go (TGen x) (TGen y) | x == y = return()

  -- unification isn't possible
  go a b = raise (UnifyError a b)

  bind p ty =
    do mb <- varBind p ty
       case mb of
         Just ty' -> go ty' ty
         Nothing  -> return ()


-- Matching Substitution -------------------------------------------------------

-- | Only allow variable bindings to variables on the lhs.  For example,
--
--   [| match [a] [Int] |] = { a |-> Int }
--
-- but
--
--   [| match [Int] [a] |] = error
--
match :: Type -> Type -> VarEnv -> Either UnifyError VarEnv
match t1 t2 env = case runM (go t1 t2) env of
                    Right (_,env') -> Right env'
                    Left err       -> Left err
  where
  go :: Type -> Type -> MGU ()
  go (TCon n1) (TCon n2)
    | n1 == n2 = return ()

  go (TApp f x) (TApp g y) =
    do go f g
       go x y

  -- only allow variables to bind on the LHS
  go (TVar a) b = bind a b

  -- generalized variables only match themselves
  go (TGen a) (TGen b)
    | a == b = return ()

  go a b = raise (MatchError a b)

  bind p ty =
    do mb <- varBind p ty
       case mb of
         Just ty' -> go ty' ty
         Nothing  -> return ()


-- Type Variable Environment ---------------------------------------------------

-- | Type variable mappings.
data VarEnv = VarEnv { veBound :: Map.Map TParam Type
                     , veFree  :: Map.Map TParam Type
                     } deriving (Show,Eq)

instance Monoid VarEnv where
  mempty = VarEnv { veBound = mempty
                  , veFree  = mempty }

  mappend l r = VarEnv { veBound = merge veBound
                       , veFree  = merge veFree }
    where
    merge p = Map.union (p l) (p r)

  mconcat us = VarEnv { veBound = merge veBound
                      , veFree  = merge veFree }
    where
    merge p = Map.unions (map p us)


bindFree :: TParam -> Type -> VarEnv
bindFree p t = mempty { veFree = Map.singleton p t }

bindFrees :: [(TParam,Type)] -> VarEnv
bindFrees us = mempty { veFree = Map.fromList us }

lookupFree :: TParam -> VarEnv -> Maybe Type
lookupFree p u = Map.lookup p (veFree u)


bindBound :: TParam -> Type -> VarEnv
bindBound p t = mempty { veBound = Map.singleton p t }

bindBounds :: [(TParam,Type)] -> VarEnv
bindBounds us = mempty { veBound = Map.fromList us }

lookupBound :: TParam -> VarEnv -> Maybe Type
lookupBound p u = Map.lookup p (veBound u)


-- | Increment the index on the bound parameters in a type, preserving the De
-- Bruijn indices.
incBinders :: Int  -- ^ Number of binders crossed
           -> Type -> Type
incBinders binders = go
  where
  go ty = case ty of
    TApp f x -> TApp (go f) (go x)
    TGen p   -> TGen p { tpIndex = tpIndex p + binders }
    TCon _   -> ty
    TVar _   -> ty


data ZonkError = OccursCheckFailed TParam Type
                 deriving (Show,Eq)

instance Pretty ZonkError where
  ppr (OccursCheckFailed p ty) =
    sep [ text "cannot construct the infinite type:"
        , pp p <+> char '=' <+> pp ty ]

-- | Zonking context
type Zonk = ReaderT VarEnv (StateT (Set.Set TParam) (ExceptionT ZonkError Id))

-- | Resolve type variables with respect to a variable environment.
zonk :: Types a => VarEnv -> a -> Either ZonkError a
zonk env a = case runM (zonkVars 0 a) env Set.empty of
  Right (a',_) -> Right a'
  Left err     -> Left err

class Types a where

  -- | Unification variables present in a type.
  typeVars :: VarEnv -> a -> Set.Set TParam

  -- | Remove type variables from a type.
  zonkVars :: Int -> a -> Zonk a

instance Types a => Types [a] where
  typeVars u = foldMap (typeVars u)
  zonkVars b = mapM (zonkVars b)

instance Types a => Types (Maybe a) where
  typeVars u = foldMap (typeVars u)
  zonkVars b = mapM (zonkVars b)


instance Types Type where

  typeVars u = go
    where
    go (TApp f x) = foldMap go [f,x]
    go (TVar p)   = maybe (Set.singleton p) go (lookupFree p u)
    go (TCon _)   = Set.empty
    go (TGen _)   = Set.empty

  zonkVars b = go
    where
    go (TApp f x) =
      do f' <- go f
         x' <- go x
         return (TApp f' x')

    go ty@(TCon _) =
      return ty

    go ty@(TVar p) =
      do u <- ask
         return (maybe ty (incBinders b) (lookupFree p u))

    go ty@(TGen p) =
      do u <- ask
         return (maybe ty (incBinders b) (lookupBound p u))
