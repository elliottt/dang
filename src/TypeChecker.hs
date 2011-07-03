module TypeChecker where

import Dang.Monad
import TypeChecker.Assume (noAssumps)
import TypeChecker.CheckKinds (kcModule)
import TypeChecker.Monad (runTC)
import Syntax.AST (Module)


-- | Top-level interface to kind checking.
kindCheckModule :: Module -> Dang Module
kindCheckModule m = runTC (kcModule noAssumps m)
