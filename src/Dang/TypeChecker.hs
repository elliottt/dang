
module Dang.TypeChecker (
    kindCheckModule
  , typeCheckModule
  ) where

import qualified Dang.Core.AST as Core
import           Dang.Monad ( Dang )
import qualified Dang.Syntax.AST as Syn
import           Dang.Utils.Panic
import           Dang.Utils.Pretty


tcPanic :: PPDoc -> a
tcPanic  = panic "Dang.TypeChecker"


-- | Top-level interface to kind checking.
kindCheckModule :: Syn.Module -> Dang Syn.Module
kindCheckModule m = tcPanic (text "kindCheckModule: not implemented")
--  logStage "kind-checker"
--  kcm <- runTC (kcModule iset m)
--  logInfo "Kind checking output:"
--  logDebug (show kcm)
--  logInfo (pretty kcm)
--  return kcm

typeCheckModule :: Syn.Module -> Dang Core.Module
typeCheckModule m = tcPanic (text "typeCheckModule: not implemented")
--  logStage "type-checker"
--  tcm <- runTC (tcModule iset m)
--  logInfo "Type checking output:"
--  logDebug (show tcm)
--  logInfo (pretty tcm)
--  return tcm
