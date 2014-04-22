
module Dang.ModuleSystem (
  scopeCheck
  ) where

import Dang.ModuleSystem.Imports ( gatherImports )
import Dang.ModuleSystem.Interface ( IfaceSet )
import Dang.Monad ( Dang, pass, logInfo )
import Dang.Syntax.AST ( Module )
import Dang.Utils.Pretty

import Data.Monoid ( mempty )

scopeCheck :: Module -> Dang (IfaceSet,Module)
scopeCheck m = pass "sc" $
  do logInfo (text "scope checking")
     _imps <- gatherImports m
     return (mempty,m)
--   logStage "module-system"
--   res@(_,m') <- runScope (scopeCheckModule m)
--   logInfo ("module-system output:\n" ++ pretty m')
--   logDebug (show m')
--   return res
