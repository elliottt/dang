
module ModuleSystem (
  scopeCheck
  ) where

import Dang.Monad (Dang)
import Dang.IO (logStage,logInfo,logDebug)
import ModuleSystem.Interface (InterfaceSet)
import ModuleSystem.ScopeCheck (runScope,scopeCheckModule)
import Pretty (pretty)
import Syntax.AST (Module)

scopeCheck :: Module -> Dang (InterfaceSet,Module)
scopeCheck m = do
  logStage "module-system"
  res@(_,m') <- runScope (scopeCheckModule m)
  logInfo ("module-system output:\n" ++ pretty m')
  logDebug (show m')
  return res
