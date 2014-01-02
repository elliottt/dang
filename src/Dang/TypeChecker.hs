
module Dang.TypeChecker where

import qualified Dang.Core.AST as Core
import           Dang.ModuleSystem.Interface ( IfaceSet )
import           Dang.Monad ( Dang )
import qualified Dang.Syntax.AST as Syn


-- | Top-level interface to kind checking.
kindCheckModule :: IfaceSet -> Syn.Module -> Dang Syn.Module
kindCheckModule iset m = return m
--  logStage "kind-checker"
--  kcm <- runTC (kcModule iset m)
--  logInfo "Kind checking output:"
--  logDebug (show kcm)
--  logInfo (pretty kcm)
--  return kcm

typeCheckModule :: IfaceSet -> Syn.Module -> Dang Core.Module
typeCheckModule iset m = undefined
--  logStage "type-checker"
--  tcm <- runTC (tcModule iset m)
--  logInfo "Type checking output:"
--  logDebug (show tcm)
--  logInfo (pretty tcm)
--  return tcm
