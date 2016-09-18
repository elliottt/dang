module Dang.Unique (
    Supply(), SupplyM(..),
    initialSupply,
    nextUnique,

    Unique(),
  ) where

import MonadLib (StateT, ExceptionT, lift)


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

instance (Monad m, SupplyM m) => SupplyM (StateT i m) where
  withSupply f = lift (withSupply f)

instance (Monad m, SupplyM m) => SupplyM (ExceptionT i m) where
  withSupply f = lift (withSupply f)


-- Uniques ---------------------------------------------------------------------

newtype Unique a = Unique Int
                   deriving (Eq,Ord,Show)
