{-# LANGUAGE Safe #-}

module Dang.ModuleSystem (
  scopeCheck
  ) where

import Dang.IO (logStage,logInfo,logDebug)
import Dang.ModuleSystem.Interface (InterfaceSet)
import Dang.ModuleSystem.ScopeCheck (runScope,scopeCheckModule)
import Dang.Monad (Dang)
import Dang.Syntax.AST (Module)
import Dang.Utils.Pretty (pretty)

scopeCheck :: Module -> Dang (InterfaceSet,Module)
scopeCheck m = do
  logStage "module-system"
  res@(_,m') <- runScope (scopeCheckModule m)
  logInfo ("module-system output:\n" ++ pretty m')
  logDebug (show m')
  return res
