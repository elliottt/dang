module Main where

import Dang.Monad
import Dang.Syntax.Location
import Dang.Syntax.Parser

import qualified Data.Text.Lazy.IO as L


main :: IO ()
main  = runDang $
  do txt     <- io (L.readFile "test.dg")
     (mb,ms) <- collectMessages (try (parseModule Interactive txt))
     io $ do mapM_ (print . locValue) (lexWithLayout (File "test.dg") txt)
             print mb
             mapM_ print (formatMessages (File "test.dg") txt ms)
