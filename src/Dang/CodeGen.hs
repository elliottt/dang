
module Dang.CodeGen (
    codeGen
  ) where

--import CodeGen.Core     as Exports
--import CodeGen.Rts      as Exports
--import CodeGen.Types    as Exports

import Dang.Core.AST (Module)
import Dang.IO (logStage)
import Dang.ModuleSystem.Interface (IfaceSet,Iface)
import Dang.Monad (Dang,whenDebugOpt,io)
import Dang.Options ( dbgDumpLLVM )
import Dang.Utils.Pretty ( PPDoc, empty )

import Text.LLVM (ppModule,runLLVM)

codeGen :: IfaceSet -> Iface -> Module ->  Dang PPDoc
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
