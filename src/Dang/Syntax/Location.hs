{-# LANGUAGE RecordWildCards #-}

module Dang.Syntax.Location (
    Source, interactive,
    SourceRange(..), HasRange(..), prettySourceRange, emptyRange,
    SourcePos(..), prettySourcePos, startPos, emptyPos,
    (<->),
    listRange
  ) where

import AlexTools
import qualified Data.Text as T
import GHC.Stack (HasCallStack)

type Source = T.Text

interactive :: Source
interactive  = T.pack "<interactive>"

emptyPos :: SourcePos
emptyPos  = startPos T.empty

emptyRange :: SourceRange
emptyRange  = SourceRange { sourceFrom = emptyPos, sourceTo = emptyPos }

listRange :: (HasCallStack,HasRange range) => [range] -> SourceRange
listRange [] = emptyRange
listRange xs = range (last xs)
