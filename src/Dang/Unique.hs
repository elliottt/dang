module Dang.Unique (
    Supply(), SupplyM(..),
    initialSupply,
    nextUnique,

    Unique(),
  ) where


-- Unique Generation -----------------------------------------------------------

newtype Supply = Supply Int
                 deriving (Show)

initialSupply :: Supply
initialSupply  = Supply 0

nextUnique :: Supply -> (Supply,Unique a)
nextUnique (Supply i) =
  let s' = Supply (i + 1)
   in s' `seq` (s',Unique i)

class SupplyM m where
  withSupply :: (Supply -> (Supply,a)) -> m a


-- Uniques ---------------------------------------------------------------------

newtype Unique a = Unique Int
                   deriving (Eq,Ord,Show)
