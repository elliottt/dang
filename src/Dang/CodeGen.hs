
module Dang.CodeGen (
    codeGen
  ) where

--import CodeGen.Core     as Exports
--import CodeGen.Rts      as Exports
--import CodeGen.Types    as Exports

import Dang.Core.AST (Module)
import Dang.ModuleSystem.Interface (IfaceSet,Iface)
import Dang.Monad
import Dang.Utils.Pretty ( PPDoc, empty )


codeGen :: IfaceSet -> Iface -> Module ->  Dang PPDoc
codeGen env iface ds = pass "cg" $
  do return empty
  {- let doc = ppModule $ snd $ runLLVM $ do
                 defineTypes
                 definePrims
                 cgDecls env iface ds
     return doc -}
