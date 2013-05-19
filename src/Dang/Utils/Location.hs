{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveFunctor #-}

module Dang.Utils.Location where

import Control.Monad (mplus)
import Data.Function (on)
import Data.List (foldl')
import Data.Monoid (Monoid(..))


-- Located Things --------------------------------------------------------------

-- | Things that carry a range in the source syntax.
data Located a = Located
  { locRange :: !Range
  , locValue ::  a
  } deriving (Show,Functor)

-- | Strip off location information.
unLoc :: Located a -> a
unLoc  = locValue

-- | Get location information from a Located thing.
getLoc :: Located a -> Range
getLoc  = locRange

-- | Extend the range of a located thing.
extendLoc :: Range -> Located a -> Located a
extendLoc r loc = loc { locRange = locRange loc `mappend` r }


-- Source Ranges ---------------------------------------------------------------

-- | A range in the program source.
data Range = Range
  { rangeStart  :: !Position
  , rangeEnd    :: !Position
  , rangeSource ::  Maybe String
  } deriving (Show)

instance Monoid Range where
  mempty = Range
    { rangeStart  = zeroPosition
    , rangeEnd    = zeroPosition
    , rangeSource = Nothing
    }

  -- widen the range
  mappend (Range ls le lmb) (Range rs re rmb) = Range
    { rangeStart  = if ls < rs then ls else rs
    , rangeEnd    = if le > re then le else re
    , rangeSource = lmb `mplus` rmb
    }

-- Positions -------------------------------------------------------------------

-- | Position information within a source.
data Position = Position
  { posOff  :: !Int
  , posLine :: !Int
  , posCol  :: !Int
  } deriving (Show,Eq)

-- | This only compares offset, assuming that the positions come from the same
-- source.
instance Ord Position where
  compare = compare `on` posOff


-- | Starting position.
zeroPosition :: Position
zeroPosition  = Position
  { posOff  = 0
  , posLine = 0
  , posCol  = 0
  }

-- | Given a character, increment a position.
movePos :: Position -> Char -> Position
movePos (Position off line col) c =
  case c of
    '\t' -> Position (off+1)  line    (col+8)
    '\n' -> Position (off+1) (line+1)  1
    _    -> Position (off+1)  line    (col+1)

-- | Move many characters at once.
movesPos :: Position -> String -> Position
movesPos pos = foldl' movePos pos
