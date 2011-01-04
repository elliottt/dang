module QualNameMap where

import QualName

import Control.Monad (msum,ap)
import Data.Serialize (Serialize(get,put),getWord8,putWord8)


type QualNameMap a = [QualNameNode a]

data QualNameNode a
  = Node Name a
  | Prefix Name (QualNameMap a)
    deriving Show

instance Serialize a => Serialize (QualNameNode a) where
  get = getWord8 >>= \tag ->
    case tag of
      0 -> Node   `fmap` get `ap` get
      1 -> Prefix `fmap` get `ap` get
      _ -> fail ("Unknown tag: " ++ show tag)
  put (Node n a)    = putWord8 0 >> put n >> put a
  put (Prefix n ts) = putWord8 1 >> put n >> put ts

empty :: QualNameMap a
empty  = []

singleton :: QualName -> a -> QualNameMap a
singleton qn = singleton' (qualPrefix qn) (qualSymbol qn)

singleton' :: Namespace -> Name -> a -> QualNameMap a
singleton' ns n a = foldl (\t p -> [Prefix p t]) [Node n a] ns

insert :: QualName -> a -> QualNameMap a -> QualNameMap a
insert qn a [] = singleton qn a
insert qn a t  = concatMap (insert' (qualPrefix qn) (qualSymbol qn) a) t

insert' :: Namespace -> Name -> a -> QualNameNode a -> QualNameMap a
insert' [] n a (Node n' _)
  | n == n'      = [Node n a]
insert' [] n a t = [t, Node n a]
insert' (p:ps) n a t@(Prefix p' ts)
  | p == p'   = [Prefix p' (concatMap (insert' ps n a) ts)]
  | otherwise = t : singleton' (p:ps) n a
insert' (p:ps) n a t@(Node n' _)
  | p == n'   = singleton' (p:ps) n a
  | otherwise = t : singleton' (p:ps) n a

lookup :: QualName -> QualNameMap a -> Maybe a
lookup qn = msum . map (lookup' (qualPrefix qn) (qualSymbol qn))

lookup' :: Namespace -> Name -> QualNameNode a -> Maybe a
lookup' [] n (Node n' a)
  | n == n'     = Just a
lookup' (p:ps) n (Prefix p' ts)
  | p == p'   = msum (map (lookup' ps n) ts)
  | otherwise = Nothing
lookup' _ _ _    = Nothing

findUnqual :: Name -> QualNameMap a -> [(QualName,a)]
findUnqual n = concatMap (loop [])
  where
  loop ps (Node n' a)
    | n == n'   = [(qualName ps n,a)]
    | otherwise = []
  loop ps (Prefix p ts) = concatMap (loop (ps ++ [p])) ts

toList :: QualNameMap a -> [(QualName,a)]
toList  = concatMap (loop [])
  where
  loop ps (Node n a)    = [(qualName ps n, a)]
  loop ps (Prefix p ts) = concatMap (loop (ps ++ [p])) ts

fromList :: [(QualName,a)] -> QualNameMap a
fromList  = foldr (uncurry insert) empty

union :: QualNameMap a -> QualNameMap a -> QualNameMap a
union as = foldr (uncurry insert) as . toList
