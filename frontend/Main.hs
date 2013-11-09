{-# LANGUAGE Trustworthy #-}

module Main where

import Dang.Compile (compile)
import Dang.FileName
import Dang.IO
import Dang.Link (link)
import Dang.ModuleSystem
import Dang.Monad
import Dang.Options
import Dang.Syntax
import Dang.TypeChecker

import Control.Monad ( unless )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )


main :: IO ()
main  =
  do opts <- parseOptions =<< getArgs
     runDang opts $ do
       logStage "dang"

       let a      = optAction opts
       logDebug ("Taking action: " ++ show a)
       let [file] = actionSources a

       m          <- loadModule file
       (iset,scm) <- scopeCheck m
       kcm        <- kindCheckModule iset scm
       tcm        <- typeCheckModule iset kcm

       compile iset tcm (ofile file)
       unless (optCompileOnly opts) (link [ofile file] (dropExtension file))
       logStage "oh-snap"
