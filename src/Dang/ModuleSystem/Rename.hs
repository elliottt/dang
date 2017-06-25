{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Dang.ModuleSystem.Rename (
    renameModule,
    renameExpr,
  ) where

import Dang.AST
import Dang.Monad
import Dang.Syntax.AST
import Dang.Syntax.Location
import Dang.ModuleSystem.Env
import Dang.ModuleSystem.Name
import Dang.Unique (SupplyM,withSupply)
import Dang.Utils.Ident (Namespace,packNamespaceLazy)
import Dang.Utils.PP
import Dang.Utils.Panic

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus)
import qualified Data.Foldable as F
import           Data.List (nub,partition)
import           Data.Maybe (catMaybes,fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           MonadLib (runM,BaseM(..),StateT,get,sets_)

type RNModule = Module (Parsed Name)

-- | Rename a top-level module.
renameModule :: HasCallStack => PModule -> Dang RNModule
renameModule Module { .. } = undefined

-- | Rename an expression.
renameExpr :: Namespace -> Expr (Parsed (SrcLoc PName)) -> Dang (Expr (Parsed Name))
renameExpr ns e = undefined
