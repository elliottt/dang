module CodeGen (
    codeGen
  ) where

--import CodeGen.Core     as Exports
--import CodeGen.Rts      as Exports
--import CodeGen.Types    as Exports

import Core.AST (Module)
import Dang.IO (logStage)
import Dang.Monad (Dang,whenDebugOpt,dbgDumpLLVM,io)
import ModuleSystem.Interface (InterfaceSet,Interface)
import Pretty (Doc,empty)

import Text.LLVM (ppModule,runLLVM)

codeGen :: InterfaceSet -> Interface -> Module ->  Dang Doc
codeGen env iface ds = do
  logStage "code-generator"
  return empty
  {-
  let doc = ppModule $ snd $ runLLVM $ do
              defineTypes
              definePrims
              cgDecls env iface ds
  whenDebugOpt dbgDumpLLVM (io (print doc))
  return doc -}
