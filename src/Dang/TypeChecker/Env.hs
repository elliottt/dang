{-# LANGUAGE Trustworthy #-}

module Dang.TypeChecker.Env where

import Dang.ModuleSystem.QualName ( Name )
import Dang.TypeChecker.Types ( Schema )

import qualified Data.Map as Map


data Env = Env { envTypes :: Map.Map Name Schema
               } deriving (Show)

emptyEnv :: Env
emptyEnv  = Env { envTypes = Map.empty }
