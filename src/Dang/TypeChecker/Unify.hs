{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Trustworthy #-}

module Dang.TypeChecker.Unify where

import Dang.TypeChecker.Types
import Dang.Utils.Pretty

import           Control.Monad ( zipWithM_ )
import           Data.Foldable ( foldMap )
import qualified Data.Map as Map
import           Data.Monoid ( Monoid(..) )
import qualified Data.Set as Set
import qualified Data.Traversable as T
import           MonadLib
                    ( runM, Id, StateT, get, set, ReaderT, ask, ExceptionT
                    , raise )


-- | A failure during unification.
data UnifyError = UnifyError Type Type
                  deriving (Show,Eq)

instance Pretty UnifyError where
  ppr (UnifyError x y) =
    hang (fsep [ text "couldn't match type", quoted (pp x)
               , text "with", quoted (pp y) ])
       2 (vcat [ text "Expected type:" <+> pp x
               , text "  Actual type:" <+> pp y ])


type MGU = StateT VarEnv (ExceptionT UnifyError Id)

-- | Compute the most-general unifier for two types, given a variable
-- environment.  Produces either an error, or an extended substitution.
mgu :: Type -> Type -> VarEnv -> Either UnifyError VarEnv
mgu inf sig env = case runM (go inf sig) env of
                    Right (_,env') -> Right env'
                    Left err       -> Left err
  where
  go :: Type -> Type -> MGU ()
  go a b = case (a,b) of

    (TApp f x, TApp g y) -> do go f g
                               go x y

    -- XXX should these be annotated with kinds?
    (TCon n1 ts1, TCon n2 ts2)
      | n1 == n2 && length ts1 == length ts2 -> zipWithM_ go ts1 ts2

    -- resolve or bind variables
    (TVar p, _) -> freeVar p b
    (_, TVar p) -> freeVar p a

    -- bound variables only unify with themselves
    (TGen x, TGen y) | x == y -> return()

    -- unification isn't possible
    _ -> raise (UnifyError a b)

  freeVar p ty =
    do u <- get
       case lookupFree p u of

         -- the variable is already bound, continue with unfication
         Just pty -> go pty ty

         -- the variable hasn't been bound, add an entry in the map
         Nothing -> set (bindFree p ty `mappend` u)


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
    TApp f x  -> TApp (go f) (go x)
    TCon n ps -> TCon n (map go ps)
    TVar _    -> ty
    TGen p    -> TGen p { tpIndex = tpIndex p + binders }


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
  zonkVars b = T.mapM (zonkVars b)

instance Types a => Types (Maybe a) where
  typeVars u = foldMap (typeVars u)
  zonkVars b = T.mapM (zonkVars b)


instance Types Type where

  typeVars u = go
    where
    go ty = case ty of
      TApp f x  -> foldMap go [f,x]
      TCon _ ps -> foldMap go ps
      TVar p    -> maybe (Set.singleton p) go (lookupFree p u)
      TGen _    -> Set.empty

  zonkVars b = go
    where
    go ty = case ty of
      TApp f x -> do f' <- go f
                     x' <- go x
                     return (TApp f' x')

      TCon n ps -> do ps' <- mapM go ps
                      return (TCon n ps')

      TVar p -> do u <- ask
                   return (maybe ty (incBinders b) (lookupFree p u))

      TGen p -> do u <- ask
                   return (maybe ty (incBinders b) (lookupBound p u))
