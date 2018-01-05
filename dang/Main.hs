{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Dang.ModuleSystem.Rename
import           Dang.Monad
import           Dang.Syntax.Format (formatMessage)
import           Dang.Syntax.Lexer
import           Dang.Syntax.Location
    (Source,SourceRange(..),SourcePos(..),HasRange(..),interactive)
import           Dang.Syntax.Parser
import qualified Dang.TypeCheck.KindCheck as KC
import           Dang.Utils.PP

import qualified Data.Foldable as F
import           Data.List (sortBy)
import           Data.Ord (comparing)
import qualified Data.Text as S
import qualified Data.Text.IO as S
import           System.Exit (exitFailure)
import           System.Environment (getArgs)


main :: IO ()
main  = runDang $
  do args <- io getArgs
     file <- case args of
               [file] -> return file
               _      -> io $ do putStrLn "Usage: dang file.dg"
                                 exitFailure

     txt <- io (S.readFile file)
     io (mapM_ (print . lexemeToken) (lexWithLayout (S.pack file) Nothing txt))

     let dumpMessages ms =
           io $ printDoc defaultConfig
              $ vcat
              $ map (formatMessage (S.pack file) txt)
              $ sortBy (comparing (sourceIndex . sourceFrom . range))
              $ F.toList ms

     (mbMod,ms) <- collectMessages $ try $
       do pMod  <- parseModule interactive txt
          rnMod <- renameModule pMod

          io (print rnMod)

          KC.checkModule rnMod

     dumpMessages ms
     io (print mbMod)
