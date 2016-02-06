module Dang.ModuleSystem.Env (
    NameTrie(),
    envDecl, envType, envMod,
    qualify,
    lookupDecl,
    lookupType,
    lookupMod,
    openMod,
    nameList,
  ) where

import Dang.Syntax.AST (PName(..))
import Dang.Utils.PP

import           Data.List (nub)
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

data NameNode a = NameNode [a] (NameTrie a)
                  deriving (Show)

instance Eq a => Monoid (NameTrie a) where
  mempty                            = NameTrie Map.empty
  mappend (NameTrie a) (NameTrie b) = NameTrie (Map.unionWith merge a b)
    where
    merge (NameNode xs x) (NameNode ys y)
      | xs == ys  = NameNode xs               (mappend x y)
      | otherwise = NameNode (nub (xs ++ ys)) (mappend x y)

  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

-- | Merge the names from the left environment, into the right environment,
-- allowing shadowing of names in the right environment.
shadowing :: NameTrie a -> NameTrie a -> NameTrie a
shadowing (NameTrie l) (NameTrie r) = NameTrie (Map.unionWith const l r)

qualify :: [L.Text] -> NameTrie a -> NameTrie a
qualify ns t = foldr step t ns
  where
  step n acc = NameTrie (Map.singleton (DefMod n) (NameNode [] acc))

envDecl, envType, envMod :: Eq a => PName -> a -> NameTrie a
envDecl = singleton DefDecl
envType = singleton DefType
envMod  = singleton DefMod

singleton :: Eq a => (L.Text -> Def) -> PName -> a -> NameTrie a
singleton mkDef pn n =
  case pn of
    PQual ns p -> qualify ns (mk p)
    PUnqual p  ->             mk p
  where
  mk p = NameTrie (Map.singleton (mkDef p) (NameNode [n] mempty))

lookupDecl, lookupType :: Eq a => PName -> NameTrie a -> [a]

lookupDecl pn t =
  case lookupPName DefDecl pn t of
    Just (NameNode as _) -> as
    Nothing              -> []

lookupType pn t =
  case lookupPName DefType pn t of
    Just (NameNode as _) -> as
    Nothing              -> []

lookupMod :: Eq a => PName -> NameTrie a -> Maybe ([a],NameTrie a)
lookupMod pn t =
  do NameNode as t' <- lookupPName DefMod pn t
     return (as,t')

lookupPName :: Eq a
            => (L.Text -> Def) -> PName -> NameTrie a -> Maybe (NameNode a)
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
openMod :: Eq a => PName -> NameTrie a -> NameTrie a
openMod n e =
  case lookupMod n e of
    Just (_,ds) -> ds `shadowing` e
    Nothing     -> e

-- | Flatten the environment into parsed names, and their values in the tree.
nameList :: NameTrie a -> [(PName,[a])]
nameList (NameTrie m) = go id (Map.toList m)
  where
  go mkNS ((d,NameNode as (NameTrie m')):rest) = names ++ go mkNS rest
    where
    names =
      case d of

        DefMod n ->
          let ts = go (mkNS . (n:)) (Map.toList m')
           in if null as
                 then                        ts
                 else (mkPName mkNS n, as) : ts

        DefDecl n -> [(mkPName mkNS n, as)]
        DefType n -> [(mkPName mkNS n, as)]

  go _ [] = []

  mkPName mkNS n =
    case mkNS [] of
      [] -> PUnqual n
      xs -> PQual xs n
