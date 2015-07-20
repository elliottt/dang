{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Dang.TypeCheck.Subst (
  Subst(),
  Types(..)
  ) where

import Dang.Core.AST

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Generics


data Subst = Subst { suFree  :: Map.Map TParam Type
                   , suBound :: Map.Map TParam Type
                   }


instance Monoid Subst where
  mempty        = Subst { suFree = Map.empty,    suBound = Map.empty }
  mappend s1 s2 = Subst { suFree = merge suFree, suBound = merge suBound }
    where
    merge p = Map.union (p s2) (fmap (apSubst s2) (p s1))


-- Substitution on Types -------------------------------------------------------

class Types a where
  apSubst :: Subst -> a -> a
  freetv  :: a -> Set.Set TParam

  default apSubst :: (Generic a, GTypes (Rep a)) => Subst -> a -> a
  apSubst su a = to (gapSubst su (from a))

  default freetv :: (Generic a, GTypes (Rep a)) => a -> Set.Set TParam
  freetv a = gfreetv (from a)

instance Types Type where
  apSubst su ty =
    case ty of
      TApp l r -> TApp (apSubst su l) (apSubst su r)
      TFree p  -> Map.findWithDefault ty p (suFree  su)
      TGen p   -> Map.findWithDefault ty p (suBound su)
      TCon{}   -> ty

  freetv ty =
    case ty of
      TFree p  -> Set.singleton p
      TApp l r -> Set.union (freetv l) (freetv r)
      TGen{}   -> Set.empty
      TCon{}   -> Set.empty

instance Types a => Types (Schema a) where
  apSubst Subst { .. } (Schema ps a) = Schema ps (apSubst su a)
    where

    -- increment all bound variables past the ones bound by this schema, as well
    -- as the domain of the bound substitution.
    su = Subst { suFree  = fmap (mapTGen incr) suFree
               , suBound = Map.mapKeysMonotonic incr
                          $ fmap (mapTGen incr) suBound }

    numBound = length ps
    incr tp  = tp { tpIndex = tpIndex tp + numBound }

  freetv (Schema _ a) = freetv a


instance Types a => Types [a]
instance Types a => Types (Maybe a)


-- Generic Substitution --------------------------------------------------------

class GTypes f where
  gapSubst :: Subst -> f a -> f a
  gfreetv  :: f a -> Set.Set TParam

instance Types c => GTypes (K1 i c) where
  gapSubst su (K1 c) = K1 (apSubst su c)
  gfreetv (K1 c)     = freetv c

instance GTypes f => GTypes (M1 i c f) where
  gapSubst su (M1 fp) = M1 (gapSubst su fp)
  gfreetv (M1 fp)     = gfreetv fp

instance GTypes U1 where
  gapSubst _ u = u
  gfreetv _    = Set.empty

instance (GTypes f, GTypes g) => GTypes (f :+: g) where
  gapSubst su (L1 fp) = L1 (gapSubst su fp)
  gapSubst su (R1 fp) = R1 (gapSubst su fp)

  gfreetv (L1 fp) = gfreetv fp
  gfreetv (R1 fp) = gfreetv fp

instance (GTypes f, GTypes g) => GTypes (f :*: g) where
  gapSubst su (fp :*: gp) = gapSubst su fp :*: gapSubst su gp
  gfreetv (fp :*: gp)     = Set.union (gfreetv fp) (gfreetv gp)
