{-# LANGUAGE ViewPatterns #-}

module Data.ClashMap (
    ClashMap()
  , Clash()
  , clashElems
  , isOk, isClash

    -- * Clash Strategies
  , Strategy
  , ok
  , clash

    -- * Construction
  , fromList, fromListWith
  , empty
  , singleton
  , insert, insertWith

    -- * Update/Delete
  , delete
  , filterWithKey

    -- * Combine
  , union, unionWith
  , intersection, intersectionWith

    -- * Query
  , lookup

    -- * Traversal
  , mapKeys

    -- * Reduction
  , foldClashMap
  , partitionClashes
  ) where

import Prelude hiding (lookup)

import Control.Arrow (second)
import Control.Monad (liftM)
import qualified Data.Monoid as M
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Traversable as T


-- Clash Map -------------------------------------------------------------------

-- | A map of values, where multiple inserts under the same name produce
-- clashes, that can be optionally resolved by a merge @Strategy@.
newtype ClashMap k a = ClashMap (Map.Map k (Clash a))
    deriving (Show)

instance Functor (ClashMap k) where
  fmap f (ClashMap m) = ClashMap (fmap (fmap f) m)

instance F.Foldable (ClashMap k) where
  fold (ClashMap m) = F.fold (fmap F.fold m)

  foldMap f (ClashMap m) = F.foldr step M.mempty m
    where
    step a z = F.foldMap f a `M.mappend` z

  foldr f z (ClashMap m) = F.foldr (flip (F.foldr f)) z m

  foldl f z (ClashMap m) = F.foldl (F.foldl f) z m

instance T.Traversable (ClashMap k) where
  traverse f (ClashMap m) = ClashMap `fmap` T.traverse (T.traverse f) m

  sequenceA (ClashMap m) = ClashMap `fmap` T.sequenceA (T.sequenceA `fmap` m)

  mapM f (ClashMap m) = liftM ClashMap (T.mapM (T.mapM f) m)

  sequence (ClashMap m) = liftM ClashMap (T.sequence (T.sequence `fmap` m))

-- | A @Clash@ value is either a single value, or a list of clashing values.
data Clash a = Ok a | Clash [a]
    deriving (Show)

instance Functor Clash where
  fmap f (Ok a)     = Ok (f a)
  fmap f (Clash as) = Clash (fmap f as)

instance F.Foldable Clash where
  fold (Ok a)     = a
  fold (Clash as) = F.fold as

  foldMap f (Ok a)     = f a
  foldMap f (Clash as) = F.foldMap f as

  foldr f z (Ok a)     = f a z
  foldr f z (Clash as) = F.foldr f z as

  foldl f z (Ok a)     = f z a
  foldl f z (Clash as) = F.foldl f z as

  foldr1 _ (Ok a)     = a
  foldr1 f (Clash as) = F.foldr1 f as

  foldl1 _ (Ok a)     = a
  foldl1 f (Clash as) = F.foldl1 f as

instance T.Traversable Clash where
  traverse f (Ok a)     = Ok `fmap` f a
  traverse f (Clash as) = Clash `fmap` T.traverse f as

  sequenceA (Ok m)     = Ok `fmap` m
  sequenceA (Clash ms) = Clash `fmap` T.sequenceA ms

  mapM f (Ok a)     = liftM Ok    (f a)
  mapM f (Clash as) = liftM Clash (T.mapM f as)

  sequence (Ok m)     = liftM Ok    m
  sequence (Clash as) = liftM Clash (T.sequence as)


-- | The elements of a @Clash@ value.
clashElems :: Clash a -> [a]
clashElems (Ok a)     = [a]
clashElems (Clash as) = as

-- | Check to see if a @Clash@ contains an ok value.
isOk :: Clash a -> Bool
isOk Ok{} = True
isOk _    = False

-- | Check to see if a @Clash@ contains clashing values.
isClash :: Clash a -> Bool
isClash Clash{} = True
isClash _       = False


-- Clash Strategies ------------------------------------------------------------

-- | A merge strategy when a @Clash@ is encountered in a @ClashMap@.
type Strategy a = a -> a -> Clash a

-- | Lift a single value.
ok :: a -> Clash a
ok  = Ok

-- | Do no resolution, and produce a @Clash@.
clash :: Strategy a
clash a b = Clash [a,b]

-- | Merge two @Clash@ values, given a merge @Strategy@.
mergeWithStrategy :: Strategy a -> Clash a -> Clash a -> Clash a
mergeWithStrategy strat = merge
  where
  merge (Ok a) (Ok b) = strat a b
  merge a      b      = Clash (clashElems a ++ clashElems b)


-- Construction ----------------------------------------------------------------

-- | Construct a @ClashMap@ from a list of key-value pairs, using the @clash@
-- @Strategy@.
fromList :: Ord k => [(k,a)] -> ClashMap k a
fromList  = fromListWith clash

-- | Construct a @ClashMap@ from a list of key-value pairs, using the provided
-- clash @Strategy@.
fromListWith :: Ord k => Strategy a -> [(k,a)] -> ClashMap k a
fromListWith strat =
  ClashMap . Map.fromListWith (mergeWithStrategy strat) . map (second ok)

-- | The empty @ClashMap@.
empty :: Ord k => ClashMap k a
empty  = ClashMap Map.empty

-- | Construct a singleton @ClashMap@.
singleton :: Ord k => k -> a -> ClashMap k a
singleton k a = ClashMap (Map.singleton k (Ok a))

-- | Insert, using the clash @Strategy@.
insert :: Ord k => k -> a -> ClashMap k a -> ClashMap k a
insert  = insertWith clash

-- | Insert, using a resolution @Strategy@.
insertWith :: Ord k => Strategy a -> k -> a -> ClashMap k a -> ClashMap k a
insertWith strat k a (ClashMap m) =
  ClashMap (Map.insertWith (mergeWithStrategy strat) k (Ok a) m)


-- Delete/Update ---------------------------------------------------------------

delete :: Ord k => k -> ClashMap k a -> ClashMap k a
delete k (ClashMap m) = ClashMap (Map.delete k m)

-- | Filter the @ClashMap@ with a predicate over keys and values.
filterWithKey :: Ord k => (k -> a -> Bool) -> ClashMap k a -> ClashMap k a
filterWithKey p (ClashMap m) = ClashMap (Map.filterWithKey p' m)
  where
  p' k (Ok a)     =      p k  a
  p' k (Clash as) = any (p k) as


-- Combine ---------------------------------------------------------------------

-- | Union, using the clash @Strategy@.
union :: Ord k => ClashMap k a -> ClashMap k a -> ClashMap k a
union  = unionWith clash

-- | Union, using a resolution @Strategy@.
unionWith :: Ord k
          => Strategy a -> ClashMap k a -> ClashMap k a -> ClashMap k a
unionWith strat (ClashMap a) (ClashMap b) =
  ClashMap (Map.unionWith (mergeWithStrategy strat) a b)

-- | Intersect, using the clash @Strategy@.
intersection :: Ord k => ClashMap k a -> ClashMap k a -> ClashMap k a
intersection  = intersectionWith clash

-- | Intersect, using a resolution @Strategy@.
intersectionWith :: Ord k
                 => Strategy a -> ClashMap k a -> ClashMap k a -> ClashMap k a
intersectionWith strat (ClashMap a) (ClashMap b) =
  ClashMap (Map.intersectionWith (mergeWithStrategy strat) a b)


-- Query -----------------------------------------------------------------------

-- | Find a @Clash@ value in a @ClashMap@.
lookup :: Ord k => k -> ClashMap k a -> Maybe (Clash a)
lookup k (ClashMap m) = Map.lookup k m


-- Traversal -------------------------------------------------------------------

mapKeys :: (Ord k, Ord k') => (k -> k') -> ClashMap k a -> ClashMap k' a
mapKeys f (ClashMap m) = ClashMap (Map.mapKeys f m)

-- Reduction -------------------------------------------------------------------

-- | Fold a @ClashMap@ resolving individual clashes.
foldClashMap :: (k -> Clash a -> b -> b) -> b -> ClashMap k a -> b
foldClashMap f z (ClashMap m) = Map.foldrWithKey f z m

-- | Partition the clashing and ok values out into key-value lists.
partitionClashes :: ClashMap k a -> ([(k,a)], [(k,[a])])
partitionClashes = foldClashMap step ([],[])
  where
  step k c (as,bs) = case clashElems c of
    [a] -> ((k,a):as,bs)
    cs  -> (as,(k,cs):bs)
