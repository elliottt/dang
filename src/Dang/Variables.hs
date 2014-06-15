{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Dang.Variables (
    Names(..)
  , FreeVars(..)
  , BoundVars(..)

  , Group(..)
  , scc
  ) where

import Dang.ModuleSystem.QualName
import Dang.Utils.Location ( Located(..) )

import Control.Applicative ( (<$>), (<*>) )
import Control.Lens ( ignored, Traversal' )
import Data.Foldable ( Foldable )
import Data.Generics ( Data, Typeable )
import Data.Graph (SCC(..))
import Data.Graph.SCC ( stronglyConnComp )
import Data.Traversable ( Traversable )
import GHC.Generics ( Rep, Generic, M1(..), K1(..), U1, (:*:)(..), (:+:)(..)
                    , from )
import GHC.Generics.Lens ( generic, _M1, _K1 )
import qualified Data.Set as Set


-- All Names -------------------------------------------------------------------

-- | A traversal for all names.
class Names a where
  names :: Traversal' a Name

  default names :: (GNames (Rep a), Generic a) => Traversal' a Name
  names = generic . gnames

instance Names Name where
  names = id

instance Names Int
instance Names Char

instance Names a => Names [a]
instance Names a => Names (Maybe a)

instance Names String where names = ignored

instance Names a => Names (Located a) where
  names f Located { .. } = Located locRange <$> names f locValue


class GNames f where
  gnames :: Traversal' (f a) Name

instance GNames f => GNames (M1 i c f) where
  gnames = _M1 . gnames

instance Names c => GNames (K1 i c) where
  gnames = _K1 . names

instance GNames U1 where
  gnames = ignored

instance (GNames f, GNames g) => GNames (f :*: g) where
  gnames t (f :*: g) = (:*:) <$> gnames t f <*> gnames t g

instance (GNames f, GNames g) => GNames (f :+: g) where
  gnames t (L1 f) = L1 <$> gnames t f
  gnames t (R1 g) = R1 <$> gnames t g


-- Free Variables --------------------------------------------------------------

class FreeVars a where
  freeVars :: a -> Set.Set Name

  default freeVars :: (GFreeVars (Rep a), Generic a) => a -> Set.Set Name
  freeVars a = gfreeVars (from a)

instance FreeVars Name where
  freeVars = Set.singleton

instance FreeVars a => FreeVars (Located a) where
  freeVars Located { .. } = freeVars locValue

instance FreeVars a => FreeVars [a]
instance FreeVars a => FreeVars (Maybe a)
instance FreeVars a => FreeVars (Group a)
instance (FreeVars a, FreeVars b) => FreeVars (a,b)
instance (FreeVars a, FreeVars b, FreeVars c) => FreeVars (a,b,c)


class GFreeVars f where
  gfreeVars :: f a -> Set.Set Name

instance GFreeVars f => GFreeVars (M1 i c f) where
  gfreeVars (M1 f) = gfreeVars f

instance FreeVars c => GFreeVars (K1 i c) where
  gfreeVars (K1 f) = freeVars f

instance GFreeVars U1 where
  gfreeVars _ = Set.empty

instance (GFreeVars f, GFreeVars g) => GFreeVars (f :*: g) where
  gfreeVars (f :*: g) = gfreeVars f `Set.union` gfreeVars g

instance (GFreeVars f, GFreeVars g) => GFreeVars (f :+: g) where
  gfreeVars (L1 f) = gfreeVars f
  gfreeVars (R1 g) = gfreeVars g


-- Bound Variables -------------------------------------------------------------

class BoundVars a where
  boundVars :: a -> Set.Set Name

  default boundVars :: (GBoundVars (Rep a), Generic a) => a -> Set.Set Name
  boundVars a = gboundVars (from a)

instance BoundVars a => BoundVars [a]
instance BoundVars a => BoundVars (Maybe a)
instance BoundVars a => BoundVars (Group a)
instance (BoundVars a, BoundVars b) => BoundVars (a,b)
instance (BoundVars a, BoundVars b, BoundVars c) => BoundVars (a,b,c)

instance BoundVars a => BoundVars (Located a) where
  boundVars Located { .. } = boundVars locValue


class GBoundVars f where
  gboundVars :: f a -> Set.Set Name

instance GBoundVars f => GBoundVars (M1 i c f) where
  gboundVars (M1 f) = gboundVars f

instance BoundVars c => GBoundVars (K1 i c) where
  gboundVars (K1 f) = boundVars f

instance GBoundVars U1 where
  gboundVars _ = Set.empty

instance (GBoundVars f, GBoundVars g) => GBoundVars (f :*: g) where
  gboundVars (f :*: g) = gboundVars f `Set.union` gboundVars g

instance (GBoundVars f, GBoundVars g) => GBoundVars (f :+: g) where
  gboundVars (L1 f) = gboundVars f
  gboundVars (R1 g) = gboundVars g


-- Strongly Connected Components -----------------------------------------------

data Group a = NonRecursive a
             | Recursive [a]
               deriving (Show,Eq,Ord,Functor,Foldable,Traversable
                        ,Generic,Data,Typeable)

toGroup :: SCC a -> Group a
toGroup s = case s of
  AcyclicSCC a -> NonRecursive a
  CyclicSCC as -> Recursive as

scc :: (FreeVars a, BoundVars a) => [a] -> [Group a]
scc as = map toGroup (stronglyConnComp graph)
  where
  graph = [ (a, n, Set.toList fvs) | a <- as
                                   , let fvs = freeVars a
                                   , n <- Set.toList (boundVars a) ]
