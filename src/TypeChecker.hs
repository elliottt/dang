module TypeChecker where

import Dang.IO (logStage,logDebug,logInfo)
import Dang.Monad (Dang)
import Interface (InterfaceSet)
import Pretty (pretty)
import Syntax.AST (Module)
import TypeChecker.CheckKinds (kcModule)
import TypeChecker.CheckTypes (tcModule)
import TypeChecker.Monad (runTC)


-- | Top-level interface to kind checking.
kindCheckModule :: InterfaceSet -> Module -> Dang Module
kindCheckModule iset m = do
  logStage "kind-checker"
  kcm <- runTC iset (kcModule m)
  logInfo "Kind checking output:"
  logDebug (show kcm)
  logInfo (pretty kcm)
  return kcm

typeCheckModule :: InterfaceSet -> Module -> Dang Module
typeCheckModule iset m = do
  logStage "type-checker"
  tcm <- runTC iset (tcModule m)
  logInfo "Type checking output:"
  logDebug (show tcm)
  logInfo (pretty tcm)
  return tcm
