module Dang.ModuleSystem.Env (
    NameTrie(),
    envDecl, envType, envMod,
    qualify,
    lookupDecl,
    lookupType,
    lookupMod,
    openMod,
    nameList,
    shadowing,
    intersectionWith,
  ) where

import Dang.Syntax.AST (PName(..))
import Dang.Utils.PP

import           Control.Monad (mplus)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as L


-- Naming Environment ----------------------------------------------------------

data Def = DefMod  !L.Text
         | DefDecl !L.Text
         | DefType !L.Text
           deriving (Eq,Ord,Show)

instance PP Def where
  ppr (DefMod  n) = ppr n
  ppr (DefDecl n) = ppr n
  ppr (DefType n) = ppr n


newtype NameTrie a = NameTrie (Map.Map Def (NameNode a))
                     deriving (Show)

data NameNode a = NameNode (Maybe a) (NameTrie a)
                  deriving (Show)

instance Monoid a => Monoid (NameTrie a) where
  mempty                            = NameTrie Map.empty
  mappend (NameTrie a) (NameTrie b) = NameTrie (Map.unionWith merge a b)
    where
    merge (NameNode xs x) (NameNode ys y) =
      NameNode (mappend xs ys) (mappend x y)

  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

-- | Merge the names from the left environment, into the right environment,
-- allowing shadowing of names in the right environment.
shadowing :: NameTrie a -> NameTrie a -> NameTrie a
shadowing (NameTrie l) (NameTrie r) = NameTrie (Map.unionWith merge l r)
  where
  merge (NameNode a l') (NameNode b r') = NameNode (a `mplus` b) (shadowing l' r')

qualify :: [L.Text] -> NameTrie a -> NameTrie a
qualify ns t = foldr step t ns
  where
  step n acc = NameTrie (Map.singleton (DefMod n) (NameNode Nothing acc))

envDecl, envType, envMod :: Monoid a => PName -> a -> NameTrie a
envDecl = singleton DefDecl
envType = singleton DefType
envMod  = singleton DefMod

singleton :: Monoid a => (L.Text -> Def) -> PName -> a -> NameTrie a
singleton mkDef pn n =
  case pn of
    PQual ns p -> qualify ns (mk p)
    PUnqual p  ->             mk p
  where
  mk p = NameTrie (Map.singleton (mkDef p) (NameNode (Just n) mempty))

lookupDecl, lookupType, lookupMod :: PName -> NameTrie a -> Maybe a

lookupDecl pn t =
  case lookupPName DefDecl pn t of
    Just (NameNode mb _) -> mb
    Nothing              -> Nothing

lookupType pn t =
  case lookupPName DefType pn t of
    Just (NameNode mb _) -> mb
    Nothing              -> Nothing

lookupMod pn t =
  case lookupPName DefMod pn t of
    Just (NameNode mb _) -> mb
    Nothing              -> Nothing

lookupPName :: (L.Text -> Def) -> PName -> NameTrie a -> Maybe (NameNode a)
lookupPName mkDef pn =
  case pn of
    PQual ns p -> go (map DefMod ns ++ [mkDef p])
    PUnqual p  -> go [mkDef p]

  where
  go (n:ns) (NameTrie m) =
    do t@(NameNode _ m') <- Map.lookup n m
       if null ns
          then return t
          else go ns m'

  go [] _ = error "Impossible"

-- | Open the module with the name N in the environment E.
openMod :: PName -> NameTrie a -> NameTrie a
openMod pn e =
  case lookupPName DefMod pn e of
    Just (NameNode _ ds) -> ds `shadowing` e
    Nothing              -> e

-- | Flatten the environment into parsed names, and their values in the tree.
nameList :: NameTrie a -> [(PName,a)]
nameList (NameTrie m) = go id (Map.toList m)
  where
  go mkNS ((d,NameNode mb (NameTrie m')):rest) = names ++ go mkNS rest
    where
    names =
      case (mb,d) of

        (Nothing, DefMod n) ->
          go (mkNS . (n:)) (Map.toList m')

        (Just a, DefMod n) ->
          let ts = go (mkNS . (n:)) (Map.toList m')
           in (mkPName mkNS n, a) : ts

        (Just a, DefDecl n) -> [(mkPName mkNS n, a)]
        (Just a, DefType n) -> [(mkPName mkNS n, a)]

        _ -> []

  go _ [] = []

  mkPName mkNS n =
    case mkNS [] of
      [] -> PUnqual n
      xs -> PQual xs n


intersectionWith :: (Maybe a -> Maybe b -> Maybe c)
                 -> NameTrie a -> NameTrie b -> NameTrie c
intersectionWith f = go
  where
  go (NameTrie l) (NameTrie r) =
    NameTrie (Map.intersectionWith merge l r)

  merge (NameNode xs l') (NameNode ys r') =
    NameNode (f xs ys) (go l' r')
