
module Dang.ModuleSystem (
  scopeCheck
  ) where

import Dang.ModuleSystem.ScopeCheck ( scopeCheckModule )
import Dang.Monad ( Dang, pass, logInfo )
import Dang.Syntax.AST ( Module )
import Dang.Utils.Pretty ( pp )


scopeCheck :: Module -> Dang Module
scopeCheck m = pass "sc" $
  do m' <- scopeCheckModule m
     logInfo (pp m')
     return m'
