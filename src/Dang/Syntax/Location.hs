{-# LANGUAGE RecordWildCards #-}

module Dang.Syntax.Location (
    SourceRange(..), HasRange(..), prettySourceRange,
    SourcePos(..), prettySourcePos,
  ) where

import AlexTools
