{-# LANGUAGE RecordWildCards #-}

module Dang.Syntax.Location (
    SrcLoc,
    SrcRange,
    Source(..),
    module Text.Location
  ) where

import Text.Location


type SrcLoc   = Located Source
type SrcRange = Range Source

-- | Input sources.
data Source = Interactive
            | File FilePath
              deriving (Eq,Ord,Show)
