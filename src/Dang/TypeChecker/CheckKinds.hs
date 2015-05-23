
module Dang.TypeChecker.CheckKinds where

import Dang.Syntax.AST (Module)
import Dang.TypeChecker.Monad (TC)


kcModule :: Module -> TC Module
kcModule  = return
