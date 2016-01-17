{-# LANGUAGE FlexibleContexts #-}

module Main where

import Dang.Monad
import Dang.Syntax.Format (printMessage)
import Dang.Syntax.Location
import Dang.Syntax.Parser
import Dang.ModuleSystem.Rename

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

     let dumpMessages ms = io (mapM_ (printMessage (File file) txt) ms)

     (mbMod,ms) <- collectMessages (try (parseModule Interactive txt))
     dumpMessages ms

     pMod <- case mbMod of
               Just pMod -> return pMod
               Nothing   -> io exitFailure

     rnMod <- renameModule pMod
     case rnMod of
       Right m   -> io (print m)
       Left errs -> do dumpMessages errs
                       io exitFailure

     io (print rnMod)


