{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Layout (
    layout
  ) where

import Syntax.Lexeme

import MonadLib
import qualified Data.Sequence as Seq


layout :: [Lexeme] -> [Lexeme]
layout  = undefined


-- Layout Levels ---------------------------------------------------------------

data Level = Level
  { levBlock  :: Bool
  , levIndent :: !Int
  } deriving Show

zeroLevel :: Level
zeroLevel  = Level { levBlock = False, levIndent = 0 }


-- Layout Processors -----------------------------------------------------------
