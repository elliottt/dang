module Compile where

import CodeGen
import Compile.LambdaLift
import Compile.Rename
import Core.AST
import Dang.IO
import Dang.Monad
import Dang.Tool
import ModuleSystem.Interface (InterfaceSet)
import Pretty

import System.IO (hPrint,hFlush)


compile :: InterfaceSet -> Module -> FilePath -> Dang ()
compile iset m out = do
  lambdaLift =<< rename m
  return ()
  {-
  rm    <- rename m
  decls <- lambdaLift iset rm
  withOpenTempFile $ \ llvm h -> do
    let iface = moduleInterface rm
    doc <- codeGen iset iface decls
    io (hPrint h doc >> hFlush h)
    logStage "compile"
    withClosedTempFile $ \ bc -> do
      sync llvm_as ["-o", bc, llvm]
      withClosedTempFile $ \ asm -> do
        sync llc       ["-o", asm, bc]
        sync assembler ["-o", out, asm]
        writeInterface iface
        -}
