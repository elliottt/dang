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

freeVarsFrom :: FreeVars a => Level -> a -> Set.Set Name
freeVarsFrom l a = Set.filter (\ n -> nameLevel n == l) (freeVars a)

freeExprVars, freeTypeVars, freeKindVars, freeSortVars
  :: FreeVars a => a -> Set.Set Name
freeExprVars  = freeVarsFrom Expr
freeTypeVars  = freeVarsFrom (Type 0)
freeKindVars  = freeVarsFrom (Type 1)
freeSortVars  = freeVarsFrom (Type 2)


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

instance (FreeVars a, FreeVars b, FreeVars c) => FreeVars (a,b,c) where
  freeVars (a,b,c) = freeVars a `Set.union` freeVars b `Set.union` freeVars c


-- Bound Variables -------------------------------------------------------------

boundVarsFrom :: BoundVars a => Level -> a -> Set.Set Name
boundVarsFrom l a = Set.filter (\n -> nameLevel n == l) (boundVars a)

boundExprVars, boundTypeVars, boundKindVars, boundSortVars
  :: BoundVars a => a -> Set.Set Name
boundExprVars  = boundVarsFrom Expr
boundTypeVars  = boundVarsFrom (Type 0)
boundKindVars  = boundVarsFrom (Type 1)
boundSortVars  = boundVarsFrom (Type 2)

class BoundVars a where
  boundVars :: a -> Set.Set Name

instance BoundVars a => BoundVars (Located a) where
  boundVars = foldMap boundVars

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
