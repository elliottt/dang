
module Dang.ModuleSystem (
  scopeCheck
  ) where

import Dang.ModuleSystem.Interface ( IfaceSet )
import Dang.Monad ( Dang )
import Dang.Syntax.AST ( Module )

import Data.Monoid ( mempty )

scopeCheck :: Module -> Dang (IfaceSet,Module)
scopeCheck m = return (mempty,m)
--   logStage "module-system"
--   res@(_,m') <- runScope (scopeCheckModule m)
--   logInfo ("module-system output:\n" ++ pretty m')
--   logDebug (show m')
--   return res
