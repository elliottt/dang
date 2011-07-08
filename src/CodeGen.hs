module CodeGen (
    codeGen
  , module Exports
  ) where

import CodeGen.Core     as Exports
import CodeGen.Rts      as Exports
import CodeGen.Types    as Exports

import Compile.LambdaLift (Decl)
import Dang.IO (logStage)
import Dang.Monad (Dang,whenDebugOpt,dbgDumpLLVM,io)
import Interface (InterfaceSet,Interface)
import Pretty (Doc)
import ReadWrite (RW)

import Text.LLVM (ppModule,runLLVM)

codeGen :: InterfaceSet -> Interface RW -> [Decl] ->  Dang Doc
codeGen env iface ds = do
  logStage "code-generator"
  let doc = ppModule $ snd $ runLLVM $ do
              defineTypes
              definePrims
              cgDecls env iface ds
  whenDebugOpt dbgDumpLLVM (io (print doc))
  return doc
