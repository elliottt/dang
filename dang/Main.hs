module Main where

import Dang.Monad
import Dang.Syntax.Lexer
import Dang.Syntax.Location
import Dang.Syntax.Parser

import qualified Data.Text.Lazy.IO as L


main :: IO ()
main  = runDang $
  do txt <- io (L.readFile "test.dg")
     io (print (lexer Interactive txt))
     m <- parseModule Interactive txt
     io (print m)
