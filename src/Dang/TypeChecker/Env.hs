{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Dang.TypeChecker.Env where

import Dang.ModuleSystem.QualName ( Name )
import Dang.TypeChecker.Types (Schema,Kind)
import Dang.TypeChecker.Subst (Types)

import           Data.Function (on)
import qualified Data.Map as Map
import           GHC.Generics (Generic)


-- Environment -----------------------------------------------------------------

data Env = Env { envTypes :: Map.Map Name Schema
               , envKinds :: Map.Map Name Kind
               } deriving (Show,Generic)

instance Monoid Env where
  mempty = Env { envTypes = Map.empty
               , envKinds = Map.empty }

  mappend a b = Env { envTypes = merge envTypes, envKinds = merge envKinds }
    where
    merge p = on Map.union p a b

instance Types Env


-- Types -----------------------------------------------------------------------

addType :: Name -> Schema -> Env -> Env
addType n s Env { .. } = Env { envTypes = Map.insert n s envTypes, .. }

addTypes :: [(Name,Schema)] -> Env -> Env
addTypes ks env = mempty { envTypes = Map.fromList ks } `mappend` env

lookupType :: Name -> Env -> Maybe Schema
lookupType n Env { .. } = Map.lookup n envTypes


-- Kinds -----------------------------------------------------------------------

addKind :: Name -> Kind -> Env -> Env
addKind n k Env { .. } = Env { envKinds = Map.insert n k envKinds, .. }

addKinds :: [(Name,Kind)] -> Env -> Env
addKinds ks env = mempty { envKinds = Map.fromList ks } `mappend` env

allKinds :: Env -> [(Name,Kind)]
allKinds Env { .. } = Map.toList envKinds

lookupKind :: Name -> Env -> Maybe Kind
lookupKind n Env { .. } = Map.lookup n envKinds
