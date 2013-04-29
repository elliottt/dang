
module ModuleSystem (
  scopeCheck
  ) where

import Dang.IO (logStage,logInfo,logDebug)
import Dang.Monad (Dang)
import Dang.Pretty (pretty)
import Dang.Syntax.AST (Module)
import ModuleSystem.Interface (InterfaceSet)
import ModuleSystem.ScopeCheck (runScope,scopeCheckModule)

scopeCheck :: Module -> Dang (InterfaceSet,Module)
scopeCheck m = do
  logStage "module-system"
  res@(_,m') <- runScope (scopeCheckModule m)
  logInfo ("module-system output:\n" ++ pretty m')
  logDebug (show m')
  return res
