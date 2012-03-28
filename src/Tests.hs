module Tests where

import Tests.Types

import Test.Framework

main :: IO ()
main  = defaultMain
  [ testGroup "types" typeTests
  ]
