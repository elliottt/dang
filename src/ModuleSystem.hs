
module ModuleSystem (
  scopeCheck
  ) where

import Dang.Monad (Dang)
import Dang.IO (logStage)
import ModuleSystem.Interface (InterfaceSet)
import ModuleSystem.ScopeCheck (runScope,scopeCheckModule)
import Syntax.AST (Module)

scopeCheck :: Module -> Dang (InterfaceSet,Module)
scopeCheck m = do
  logStage "module-system"
  runScope (scopeCheckModule m)
