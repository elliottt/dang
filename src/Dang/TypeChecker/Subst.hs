{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Dang.TypeChecker.Subst (
  Subst(), listFreeSubst, listBoundSubst, (@@), merge,
  Types(..),
  ) where

import           Dang.TypeChecker.Types (Type(..),TParam)

import           Control.Monad (guard)
import qualified Data.Map.Strict as Map
import           GHC.Generics
                     ( Rep, Generic, M1(..), K1(..), U1, (:*:)(..), (:+:)(..)
                     , from, to )


-- Substitutions ---------------------------------------------------------------

data Subst = Subst { substFree, substBound :: Map.Map TParam Type
                   } deriving (Show)

instance Monoid Subst where
  mempty  = Subst mempty mempty
  mappend = (@@)

listFreeSubst :: [(TParam,Type)] -> Subst
listFreeSubst xs = mempty { substFree = Map.fromList xs }

listBoundSubst :: [(TParam,Type)] -> Subst
listBoundSubst xs = mempty { substBound = Map.fromList xs }

-- | Compose two substitutions.  The left substitution will be applied to the
-- right before merging the two together.
(@@) :: Subst -> Subst -> Subst
l @@ r = Subst { substFree = comp substFree, substBound = comp substBound }
  where
  comp p = (apply l `fmap` p r) `Map.union` p l

-- | Merge two substitutions that agree where they overlap.
merge :: Subst -> Subst -> Maybe Subst
merge s1 s2 =
  do substFree  <- mergeMap substFree  TVar
     substBound <- mergeMap substBound TGen
     return Subst { .. }
  where
  mergeMap p mkTy =
    do let vars    = Map.keys (Map.intersection (p s1) (p s2))
           agree v = apply s1 (mkTy v) == apply s2 (mkTy v)
       guard (all agree vars)
       return (Map.union (p s1) (p s2))


-- Application -----------------------------------------------------------------

class Types a where
  -- | Apply a type substitution.
  apply :: Subst -> a -> a

  default apply :: (Generic a, GTypes (Rep a)) => Subst -> a -> a
  apply su a = to (gapply su (from a))

instance Types Type where
  apply s (TApp a b) = TApp (apply s a) (apply s b)
  apply _ t@TCon{}   = t
  apply s t@(TVar p) = Map.findWithDefault t p (substFree  s)
  apply s t@(TGen p) = Map.findWithDefault t p (substBound s)

instance Types a => Types [a]
instance Types a => Types (Maybe a)
instance (Types a, Types b) => Types (a,b)


class GTypes f where
  gapply :: Subst -> f a -> f a

instance Types c => GTypes (K1 i c) where
  gapply su (K1 c) = K1 (apply su c)

instance GTypes f => GTypes (M1 i c f) where
  gapply su (M1 fp) = M1 (gapply su fp)

instance GTypes U1 where
  gapply _ u = u

instance (GTypes f, GTypes g) => GTypes (f :+: g) where
  gapply su (L1 fp) = L1 (gapply su fp)
  gapply su (R1 fp) = R1 (gapply su fp)

instance (GTypes f, GTypes g) => GTypes (f :*: g) where
  gapply su (fp :*: gp) = gapply su fp :*: gapply su gp
