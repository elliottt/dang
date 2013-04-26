module Dang.Compile where

import CodeGen
import Dang.Compile.LambdaLift
import Dang.Compile.Rename
import Core.AST
import Core.Interface (moduleInterface)
import Dang.IO
import Dang.Monad
import Dang.Tool
import ModuleSystem.Interface (InterfaceSet,Interface,writeInterface)
import Pretty

import System.IO (hPrint,hFlush)


compile :: InterfaceSet -> Module -> FilePath -> Dang ()
compile iset m out = do
  lambdaLift =<< rename m
  writeInterface (moduleInterface m)
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
