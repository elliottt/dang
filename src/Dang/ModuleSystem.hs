
module Dang.ModuleSystem (
  scopeCheck
  ) where

import Dang.ModuleSystem.ScopeCheck ( scopeCheckModule )
import Dang.Monad ( Dang, pass )
import Dang.Syntax.AST ( Module )


scopeCheck :: Module -> Dang Module
scopeCheck m = pass "sc" (scopeCheckModule m)
