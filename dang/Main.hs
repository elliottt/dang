{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Dang.ModuleSystem.Rename
import           Dang.Monad
import           Dang.Syntax.Format (formatMessage)
import           Dang.Syntax.Location (Source(..),thing,Range(..),getLoc)
import           Dang.Syntax.Parser
import qualified Dang.TypeCheck.KindCheck as KC
import           Dang.Utils.PP

import qualified Data.Foldable as F
import           Data.List (sortBy)
import           Data.Ord (comparing)
import qualified Data.Text.Lazy.IO as L
import           System.Exit (exitFailure)
import           System.Environment (getArgs)


main :: IO ()
main  = runDang $
  do args <- io getArgs
     file <- case args of
               [file] -> return file
               _      -> io $ do putStrLn "Usage: dang file.dg"
                                 exitFailure

     txt        <- io (L.readFile file)
     io (mapM_ (print . thing) (lexWithLayout (File file) Nothing txt))

     let dumpMessages ms =
           io $ printDoc defaultConfig
              $ vcat
              $ map (formatMessage (File file) txt)
              $ sortBy (comparing (rangeStart . getLoc))
              $ F.toList ms

     (mbMod,ms) <- collectMessages $ try $
       do pMod  <- parseModule Interactive txt
          rnMod <- renameModule pMod
          KC.checkModule rnMod

     dumpMessages ms
     io (print mbMod)


