module Tests.Utils where

import Control.Monad (filterM)
import Test.QuickCheck

reduce :: [a] -> Gen [a]
reduce  = filterM (const (choose (True,False)))
