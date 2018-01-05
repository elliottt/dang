module Dang.ModuleSystem.Env (
    NameTrie(),
    NameNode(..),
    Def(..),
    envDecl, envType, envMod,
    qualify,
    insertPName,
    lookupDecl,
    lookupType,
    lookupMod,
    lookupPName,
    openMod,
    shadowing,
    intersectionWith,
  ) where

import Dang.Syntax.AST (PName(..))
import Dang.Utils.PP
import Dang.Utils.Panic (panic)

import           Control.Monad (mplus)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T


-- Naming Environment ----------------------------------------------------------

data Def = DefMod  !T.Text
         | DefDecl !T.Text
         | DefType !T.Text
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

qualify :: [T.Text] -> NameTrie a -> NameTrie a
qualify ns t = foldr step t ns
  where
  step n acc = NameTrie (Map.singleton (DefMod n) (NameNode Nothing acc))

envDecl, envType, envMod :: Monoid a => PName -> a -> NameTrie a
envDecl = singleton DefDecl
envType = singleton DefType
envMod  = singleton DefMod

singleton :: Monoid a => (T.Text -> Def) -> PName -> a -> NameTrie a
singleton mkDef pn n =
  case pn of
    PQual _ ns p -> qualify ns (mk p)
    PUnqual _ p  ->             mk p
  where
  mk p = NameTrie (Map.singleton (mkDef p) (NameNode (Just n) mempty))


insertPName :: Monoid a => (T.Text -> Def) -> PName -> a -> NameTrie a -> NameTrie a
insertPName mkDef pn a =
  case pn of
    PUnqual _ p  -> go (mkDef p) []
    PQual _ ns p ->
      case map DefMod ns ++ [mkDef p] of
        n:ns' -> go n ns'
        _     -> panic (text "Invalid qualified name")


  where
  go n ns (NameTrie m) = NameTrie (Map.alter upd n m)
    where
    upd mb =
      case ns of
        n':rest ->
          case mb of
            Just (NameNode x sub) -> Just (NameNode x       (go n' rest sub))
            Nothing               -> Just (NameNode Nothing (go n' rest mempty))

        [] ->
          case mb of
            Just (NameNode x sub) -> Just (NameNode (Just a `mappend` x) sub)
            Nothing               -> Just (NameNode (Just a)             mempty)


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

lookupPName :: (T.Text -> Def) -> PName -> NameTrie a -> Maybe (NameNode a)
lookupPName mkDef pn =
  case pn of
    PQual _ ns p -> go (map DefMod ns ++ [mkDef p])
    PUnqual _ p  -> go [mkDef p]

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


intersectionWith :: (Maybe a -> Maybe b -> Maybe c)
                 -> NameTrie a -> NameTrie b -> NameTrie c
intersectionWith f = go
  where
  go (NameTrie l) (NameTrie r) =
    NameTrie (Map.intersectionWith merge l r)

  merge (NameNode xs l') (NameNode ys r') =
    NameNode (f xs ys) (go l' r')
