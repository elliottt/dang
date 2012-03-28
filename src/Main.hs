module Main where

import Compile (compile)
import Dang.IO
import Dang.FileName
import Dang.Monad
import Desugar (desugar)
import Link (link)
import ModuleSystem
import Syntax
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
  dsm        <- desugar scm
  kcm        <- kindCheckModule iset dsm
  tcm        <- typeCheckModule iset kcm

  compile iset tcm (ofile file)
  unless (optCompileOnly opts) (link [ofile file] (dropExtension file))
  logStage "oh-snap"
