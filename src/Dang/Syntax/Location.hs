{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module Dang.Syntax.Location where

import           Dang.Utils.PP

import           Control.Applicative ((<|>))
import           Control.Lens (Plated,transform)
import           Data.Function (on)
import           Data.Int (Int64)
import qualified Data.Text.Lazy as L
import           GHC.Generics


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


unLocAll :: (Plated a, UnLoc a) => a -> a
unLocAll  = transform unLoc

-- | Remove one layer of location information.
class UnLoc a where
  unLoc :: a -> a

-- We can't remove location information from a located thing, but we can remove
-- location information from the inner thing.
instance UnLoc a => UnLoc (Located a) where
  unLoc = fmap unLoc

instance UnLoc a => UnLoc [a] where
  unLoc = fmap unLoc

instance UnLoc a => UnLoc (Maybe a) where
  unLoc = fmap unLoc


class HasLoc a where
  getLoc :: a -> Range

instance HasLoc a => HasLoc [a] where
  getLoc = foldMap getLoc

instance (HasLoc a, HasLoc b) => HasLoc (a,b) where
  getLoc (a,b) = mappend (getLoc a) (getLoc b)

instance HasLoc Range where
  getLoc = id

instance HasLoc (Located a) where
  getLoc = locRange

at :: HasLoc loc => a -> loc -> Located a
at locValue loc = Located { locRange = getLoc loc, .. }

thing :: Located a -> a
thing Located { .. } = locValue


-- | Move a position by the width of a character.
movePos :: Char -> Position -> Position
movePos c p
  | c == '\t' = p { posCol = posCol p + 8, posOff = posOff p + 8 }
  | c == '\n' = p { posRow = posRow p + 1, posCol = 1, posOff = posOff p + 1 }
  | c == '\r' = p
  | otherwise = p { posCol = posCol p + 1, posOff = posOff p + 1 }


zeroPos :: Position
zeroPos  = Position { posRow = 1, posCol = 1, posOff = 0 }

instance Eq Position where
  (==) = (==) `on` posOff
  (/=) = (/=) `on` posOff

instance Ord Position where
  compare = compare `on` posOff


-- | The lines that the region describes, with optional additional lines of
-- context.
rangeText :: Int -> Range -> L.Text -> L.Text
rangeText cxt Range { .. } txt = L.unlines
                               $ take len
                               $ drop start
                               $ L.lines txt
  where
  start = max 0 (fromIntegral (posRow rangeStart) - cxt - 1)
  len   = max 1 (cxt + fromIntegral (posRow rangeEnd - posRow rangeStart) + 1)

-- | Generate an underline for the range mentioned.
rangeUnderline :: Range -> Doc
rangeUnderline Range { .. } = text (replicate (start - 1) ' ') <> text line
  where
  start = fromIntegral (posCol rangeStart)
  end   = fromIntegral (posCol rangeEnd)

  len   = end - start

  line | len > 1   = replicate len '~'
       | otherwise = "^"

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
