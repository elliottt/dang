{-# LANGUAGE DeriveGeneric #-}
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
import           GHC.Generics (Generic)


data Position = Position { posRow, posCol, posOff :: !Int64
                         } deriving (Show,Generic)

data Range = Range { rangeSource :: Maybe Source
                   , rangeStart, rangeEnd :: !Position
                   } deriving (Show,Eq,Ord,Generic)

data Source = Interactive
            | File FilePath
              deriving (Show,Eq,Ord,Generic)

data Located a = Located { locRange :: !Range
                         , locValue :: a
                         } deriving (Functor,Foldable,Traversable,Show,Eq,Ord
                                    ,Generic)


class HasLoc a where
  getLoc :: a -> Range

instance HasLoc Range where
  getLoc = id

instance HasLoc (Located a) where
  getLoc = locRange

at :: HasLoc loc => loc -> a -> Located a
at loc locValue = Located { locRange = getLoc loc, .. }

thing :: Located a -> a
thing Located { .. } = locValue


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
rangeText :: Range -> L.Text -> L.Text
rangeText Range { .. } txt = L.take (posOff rangeEnd - posOff rangeStart)
                           $ L.drop (posOff rangeStart) txt

instance Monoid Range where
  mempty = Range { rangeSource = Nothing
                 , rangeStart  = zeroPos
                 , rangeEnd    = zeroPos }

  mappend (Range s1 l1 r1) (Range s2 l2 r2)
    | l1 > r2   = Range (s1 <|> s2) l2 r1
    | otherwise = Range (s1 <|> s2) l1 r2


instance PP Position where
  ppr Position { .. } = ppr posRow <> char ':' <> ppr posCol

instance PP Range where
  ppr Range { .. } = ppr rangeStart <> char '-' <> ppr rangeEnd

instance PP Source where
  ppr Interactive = text "<interactive>"
  ppr (File path) = text path

-- | By default, don't print any location information for a Located thing.
instance PP a => PP (Located a) where
  ppr Located { .. } = ppr locValue
