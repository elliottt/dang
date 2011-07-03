{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeChecker.Unify where

import Dang.Monad
import Pretty
import TypeChecker.Types
import Utils ((!!?))

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import MonadLib (ExceptionM)
import qualified Data.Set as Set


newtype Subst = Subst { unSubst :: [(TParam,Type)] }
    deriving (Show)

instance Pretty Subst where
  pp _ (Subst us) = braces (commas (map step us))
    where
    step (p,ty) = ppr p <+> text "+->" <+> ppr ty

class Types a where
  apply    :: Subst -> a -> a
  typeVars :: a -> Set.Set TParam

instance Types Type where
  apply s ty = case ty of
    TApp f x     -> TApp (apply s f) (apply s x)
    TInfix n l r -> TInfix n (apply s l) (apply s r)
    TVar _ p     -> fromMaybe ty (lookupSubst p s)
    TGen{}       -> ty
    TNat{}       -> ty
    TCon{}       -> ty

  typeVars ty = case ty of
    TApp f x     -> typeVars f `Set.union` typeVars x
    TInfix _ l r -> typeVars l `Set.union` typeVars r
    TVar _ p     -> Set.singleton p
    TGen{}       -> Set.empty
    TNat{}       -> Set.empty
    TCon{}       -> Set.empty

lookupSubst :: TParam -> Subst -> Maybe Type
lookupSubst p (Subst u) = lookup p u

emptySubst :: Subst
emptySubst  = Subst []

-- | Generate a singleton substitution.
(+->) :: TParam -> Type -> Subst
v +-> ty = Subst [(v,ty)]

-- | Compose two substitutions.
(@@) :: Subst -> Subst -> Subst
s1 @@ Subst s2 = Subst ([ (p,apply s1 ty) | (p,ty) <- s2 ] ++ unSubst s1)

data UnifyError
  = UnifyError Type Type
  | UnifyOccursCheck TParam Type
  | UnifyGeneric String
    deriving (Show,Typeable)

instance Exception UnifyError

unifyError :: ExceptionM m SomeException => String -> m a
unifyError  = raiseE . UnifyGeneric

-- | Generate the most-general unifier for two types.
mgu :: ExceptionM m SomeException => Type -> Type -> m Subst
mgu a b = case (a,b) of

  -- type application
  (TApp f x, TApp g y) -> do
    sf <- mgu f g
    sx <- mgu x y
    return (sf @@ sx)

  -- infix type constructor application
  (TInfix n l r, TInfix m x y) -> do
    unless (n == m) $ unifyError $ concat
      [ "Expected infix constructor ``", pretty n
      , "'', got ``", pretty m, "''" ]
    sl <- mgu l x
    sr <- mgu r y
    return (sl @@ sr)

  -- variables
  (TVar _ p, r) -> varBind p r
  (l, TVar _ p) -> varBind p l

  -- constructors
  (TCon l, TCon r) | l == r -> return emptySubst

  -- how do we handle nats here?

  _ -> raiseE (UnifyError a b)


-- | Generate a substitution that unifies a variable with a type.
--
-- XXX should this do a kind check in addition to an occurs check?
varBind :: ExceptionM m SomeException => TParam -> Type -> m Subst
varBind p ty
  | isTVar ty                  = return emptySubst
  | p `Set.member` typeVars ty = raiseE (UnifyOccursCheck p ty)
  | otherwise                  = return (p +-> ty)


-- Instantiation ---------------------------------------------------------------

data InstError = InstError TParam
    deriving (Show,Typeable)

instance Exception InstError

class Instantiate t where
  inst :: ExceptionM m SomeException => [Type] -> t -> m t

instance Instantiate a => Instantiate [a] where
  inst ts = mapM (inst ts)

instance Instantiate Type where
  inst ts (TApp l r) = do
    l' <- inst ts l
    r' <- inst ts r
    return (TApp l' r')

  inst ts (TInfix n l r) = do
    l' <- inst ts l
    r' <- inst ts r
    return (TInfix n l' r')

  inst ts (TGen n p) = case ts !!? n of
    Nothing -> raiseE (InstError p)
    Just ty -> return ty

  inst _ ty = return ty


-- Quantification --------------------------------------------------------------

quantify :: [TParam] -> Type -> Forall Type
quantify ps ty = Forall vs (apply s ty)
  where
  vs = [ v | v <- Set.toList (typeVars ty), v `elem` ps ]
  s  = Subst (zipWith (\n p -> (p,TGen n p)) [0 ..] vs)