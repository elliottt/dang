module TypeChecker where

import Dang.IO (logStage,logDebug,logInfo)
import Dang.Monad (Dang)
import Pretty (pretty)
import Syntax.AST (Module)
import TypeChecker.Assume (noAssumps)
import TypeChecker.CheckKinds (kcModule)
import TypeChecker.Monad (runTC)


-- | Top-level interface to kind checking.
kindCheckModule :: Module -> Dang Module
kindCheckModule m = do
  logStage "kind-checker"
  kcm <- runTC (kcModule noAssumps m)
  logInfo "Kind checking output:"
  logDebug (show kcm)
  logInfo (pretty kcm)
  return kcm
