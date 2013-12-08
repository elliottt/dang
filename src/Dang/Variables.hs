{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dang.Variables where

import Dang.ModuleSystem.QualName
import Dang.Utils.Location

import Data.Foldable ( foldMap )
import Data.Graph (SCC(..))
import Data.Graph.SCC (stronglyConnComp)
import qualified Data.Set as Set


-- Free Variables --------------------------------------------------------------

class FreeVars a where
  freeVars :: a -> Set.Set Name

instance FreeVars a => FreeVars (Located a) where
  freeVars = foldMap freeVars

instance FreeVars a => FreeVars (Maybe a) where
  freeVars = foldMap freeVars

instance FreeVars a => FreeVars [a] where
  freeVars = foldMap freeVars

instance FreeVars a => FreeVars (Set.Set a) where
  freeVars = foldMap freeVars

instance (FreeVars a, FreeVars b) => FreeVars (a,b) where
  freeVars (a,b) = freeVars a `Set.union` freeVars b


-- Bound Variables -------------------------------------------------------------

class BoundVars a where
  boundVars :: a -> Set.Set Name

instance BoundVars a => BoundVars (Maybe a) where
  boundVars = foldMap boundVars

instance BoundVars a => BoundVars [a] where
  boundVars = foldMap boundVars

instance BoundVars a => BoundVars (Set.Set a) where
  boundVars = foldMap boundVars


-- Strongly Connected Components -----------------------------------------------

data Group a = NonRecursive a
             | Recursive [a]
               deriving (Show,Eq,Ord)

flattenGroup :: Group a -> [a]
flattenGroup g = case g of
  NonRecursive a -> [a]
  Recursive as   -> as

toGroup :: SCC a -> Group a
toGroup s = case s of
  AcyclicSCC a -> NonRecursive a
  CyclicSCC as -> Recursive as

sccFreeNames :: (FreeVars a, BoundVars a) => [a] -> [Group a]
sccFreeNames as = map toGroup (stronglyConnComp graph)
  where
  graph = [ (a, n, Set.toList fvs) | a <- as
                                   , let fvs = freeVars a
                                   , n <- Set.toList (boundVars a) ]
