module Main where

import Tests.Unification

import Test.Framework

main :: IO ()
main  = defaultMain
  [ unificationTests
  ]
