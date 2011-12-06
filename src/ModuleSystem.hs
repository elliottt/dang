
module ModuleSystem (
  scopeCheck
  ) where

import Dang.Monad (Dang)
import ModuleSystem.Interface (InterfaceSet)
import ModuleSystem.ScopeCheck (runScope,scopeCheckModule)
import Syntax.AST (Module)

scopeCheck :: Module -> Dang (InterfaceSet,Module)
scopeCheck m = runScope (scopeCheckModule m)
