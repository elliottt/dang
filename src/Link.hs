module Link where

import Dang.IO
import Dang.Monad
import Dang.Tool

import MonadLib
import System.Directory (removeFile)
import System.FilePath ((<.>))


rtsPath :: FilePath
rtsPath  = "rts/librts.a"

link :: [FilePath] -> FilePath -> Dang ()
link ofiles out =
  withClosedTempFile $ \ res ->
  withClosedTempFile $ \ asm ->
  withClosedTempFile $ \ obj -> do
    sync llvm_ld ("-o" : res : rtsPath : ofiles)
    sync llc ["-o", asm, res <.> "bc"]
    sync assembler ["-o", obj, asm]
    opts <- ask
    unless (optKeepTempFiles opts) (io (removeFile (res <.> "bc")))
    sync gcc ["-o", out, obj]
