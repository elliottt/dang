{-# LANGUAGE TypeSynonymInstances #-}

module TypeChecker.Env where

import Core.AST (Term)
import QualName (QualName)
import Pretty (Pretty(pp),declBlock,ppr,(<+>),text)
import TypeChecker.Unify (Types(..))

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set

type Assumps ty = Map.Map QualName (Assump ty)

instance Pretty ty => Pretty (Assumps ty) where
  pp _ = declBlock . map step . Map.toList
    where
    step (n,a) = ppr n <+> text "+->" <+> ppr a

instance Types ty => Types (Assumps ty) where
  apply' b u  = Map.map (apply' b u)
  typeVars = Fold.foldl step Set.empty
    where
    step acc a = acc `Set.union` typeVars a

data Assump ty = Assump
  { aBody :: Maybe Term
  , aData :: ty
  } deriving (Show)

instance Pretty ty => Pretty (Assump ty) where
  pp _ = ppr . aData

instance Types ty => Types (Assump ty) where
  apply' b u a = a { aData = apply' b u (aData a) }
  typeVars  = typeVars . aData

singletonAssump :: QualName -> Assump ty -> Assumps ty
singletonAssump  = Map.singleton

emptyAssumps :: Assumps ty
emptyAssumps  = Map.empty

lookupAssump :: QualName -> Assumps ty -> Maybe (Assump ty)
lookupAssump  = Map.lookup

addAssump :: QualName -> Assump ty -> Assumps ty -> Assumps ty
addAssump  = Map.insert

mergeAssumps :: Assumps ty -> Assumps ty -> Assumps ty
mergeAssumps  = Map.union

assumps :: Assumps ty -> [ty]
assumps  = map aData . Map.elems
