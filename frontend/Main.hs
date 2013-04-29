module Main where

import Dang.Compile (compile)
import Dang.FileName
import Dang.IO
import Dang.Monad
import Dang.Syntax
import Link (link)
import ModuleSystem
import TypeChecker

import MonadLib
import System.FilePath (dropExtension)

main :: IO ()
main  = runDang $ do
  logStage "dang"
  opts <- ask

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
