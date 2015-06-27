{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module Dang.Syntax.Location where

import           Dang.Utils.PP

import           Control.Applicative ((<|>))
import           Data.Function (on)
import           Data.Int (Int64)
import qualified Data.Text.Lazy as L


data Position = Position { posRow, posCol, posOff :: !Int64
                         } deriving (Show)

data Range = Range { rangeSource :: Maybe Source
                   , rangeStart, rangeEnd :: !Position
                   } deriving (Show,Eq,Ord)

data Source = Source { srcText   :: L.Text
                       -- ^ The full text of the origin
                     , srcOrigin :: String
                       -- ^ Origin of the text (file, interactive, etc...)
                     } deriving (Show,Eq,Ord)

data Located a = Located { locRange :: !Range
                         , locValue :: a
                         } deriving (Functor,Foldable,Traversable,Show,Eq,Ord)


class HasLoc a where
  getLoc :: a -> Range

at :: HasLoc loc => loc -> a -> Located a
at loc locValue = Located { locRange = getLoc loc, .. }

thing :: Located a -> a
thing Located { .. } = locValue

-- | By default, don't print any location information for a Located thing.
instance PP a => PP (Located a) where
  ppr Located { .. } = ppr locValue


-- | Move a position by the width of a character.
movePos :: Char -> Position -> Position
movePos c p
  | c == '\t' = p { posCol = posCol p + 8, posOff = posOff p + 8 }
  | c == '\n' = p { posRow = posRow p + 1, posCol = 0, posOff = posOff p + 1 }
  | c == '\r' = p
  | otherwise = p { posCol = posCol p + 1, posOff = posOff p + 1 }


zeroPos :: Position
zeroPos  = Position { posRow = 1, posCol = 1, posOff = 0 }

instance Eq Position where
  (==) = (==) `on` posOff
  (/=) = (/=) `on` posOff

instance Ord Position where
  compare = compare `on` posOff


-- | The text that the region describes
rangeText :: Range -> L.Text
rangeText Range { .. } =
  case srcText `fmap` rangeSource of
    Just txt -> L.take (posOff rangeEnd - posOff rangeStart)
              $ L.drop (posOff rangeStart) txt
    Nothing  -> L.empty

instance Monoid Range where
  mempty = Range { rangeSource = Nothing
                 , rangeStart  = zeroPos
                 , rangeEnd    = zeroPos }

  mappend a@(Range s1 l1 r1) b@(Range s2 l2 r2)
    | l1 > r2   = Range (s1 <|> s2) r2 l1
    | otherwise = Range (s1 <|> s2) l1 r2
