module Tests.Monadic where

import Dang.Monad (runDangWithArgs,Dang)

import Control.Exception as X
import Control.Monad (unless)


-- | Run a @Dang@ computation that is expected to fail.
assertFailure :: [String] -> Dang a -> IO ()
assertFailure args m = do
  let handler :: SomeException -> IO Bool
      handler _ = return True
  failed <- (runDangWithArgs args m >> return False) `X.catch` handler
  unless failed (fail "Unexpected success")
