{-# LANGUAGE Safe #-}

module Dang.TypeChecker where

import Dang.IO (logStage,logDebug,logInfo)
import Dang.ModuleSystem.Interface (InterfaceSet)
import Dang.Monad (Dang)
import Dang.TypeChecker.CheckKinds (kcModule)
import Dang.TypeChecker.CheckTypes (tcModule)
import Dang.TypeChecker.Monad (runTC)
import Dang.Utils.Pretty (pretty)
import qualified Dang.Core.AST   as Core
import qualified Dang.Syntax.AST as Syn


-- | Top-level interface to kind checking.
kindCheckModule :: InterfaceSet -> Syn.Module -> Dang Syn.Module
kindCheckModule iset m = do
  logStage "kind-checker"
  kcm <- runTC (kcModule iset m)
  logInfo "Kind checking output:"
  logDebug (show kcm)
  logInfo (pretty kcm)
  return kcm

typeCheckModule :: InterfaceSet -> Syn.Module -> Dang Core.Module
typeCheckModule iset m = do
  logStage "type-checker"
  tcm <- runTC (tcModule iset m)
  logInfo "Type checking output:"
  logDebug (show tcm)
  logInfo (pretty tcm)
  return tcm
