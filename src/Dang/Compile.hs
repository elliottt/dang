{-# LANGUAGE Trustworthy #-}

module Dang.Compile where

import Dang.Compile.LambdaLift ( lambdaLift )
import Dang.Core.AST ( Module )
import Dang.Core.Interface ( moduleIface )
import Dang.ModuleSystem.Interface ( writeIface )
import Dang.Monad


compile :: Module -> FilePath -> Dang ()
compile m out = do
  lambdaLift m
  writeIface (moduleIface m)
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
