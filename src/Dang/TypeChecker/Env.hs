{-# LANGUAGE RecordWildCards #-}

module Dang.TypeChecker.Env where

import Dang.ModuleSystem.QualName ( Name )
import Dang.TypeChecker.Types (Schema,Kind)

import qualified Data.Map as Map


-- Environment -----------------------------------------------------------------

data Env = Env { envTypes :: Map.Map Name Schema
               , envKinds :: Map.Map Name Kind
               } deriving (Show)

emptyEnv :: Env
emptyEnv  = Env { envTypes = Map.empty
                , envKinds = Map.empty }

-- Types -----------------------------------------------------------------------

addType :: Name -> Schema -> Env -> Env
addType n s Env { .. } = Env { envTypes = Map.insert n s envTypes, .. }

lookupType :: Name -> Env -> Maybe Schema
lookupType n Env { .. } = Map.lookup n envTypes


-- Kinds -----------------------------------------------------------------------

addKind :: Name -> Kind -> Env -> Env
addKind n k Env { .. } = Env { envKinds = Map.insert n k envKinds, .. }

lookupKind :: Name -> Env -> Maybe Kind
lookupKind n Env { .. } = Map.lookup n envKinds
