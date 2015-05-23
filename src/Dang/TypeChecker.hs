
module Dang.TypeChecker (
    kindCheckModule
  , typeCheckModule
  ) where

import qualified Dang.Core.AST as Core
import           Dang.Monad (Dang,pass,logInfo)
import qualified Dang.Syntax.AST as Syn
import           Dang.TypeChecker.CheckKinds (kcModule)
import           Dang.TypeChecker.Monad (TC,runTC)
import           Dang.Utils.Panic
import           Dang.Utils.Pretty


tcPanic :: PPDoc -> a
tcPanic  = panic "Dang.TypeChecker"


-- | Top-level interface to kind checking.
kindCheckModule :: Syn.Module -> Dang Syn.Module
kindCheckModule m = pass "kc" $
  do kcm <- runTC (kcModule m)
     logInfo (text "Kind checking output:" $$ pp kcm)
     return kcm

typeCheckModule :: Syn.Module -> Dang Core.Module
typeCheckModule m = tcPanic (text "typeCheckModule: not implemented")
--  logStage "type-checker"
--  tcm <- runTC (tcModule iset m)
--  logInfo "Type checking output:"
--  logDebug (show tcm)
--  logInfo (pretty tcm)
--  return tcm
