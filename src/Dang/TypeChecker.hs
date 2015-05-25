
module Dang.TypeChecker (typeCheckModule) where

import qualified Dang.Core.AST as Core
import           Dang.Monad (Dang,pass,logInfo)
import qualified Dang.Syntax.AST as Syn
import           Dang.TypeChecker.CheckTypes (tcModule)
import           Dang.TypeChecker.Monad (TC,runTC)
import           Dang.Utils.Panic
import           Dang.Utils.Pretty


-- | Type-check a module, producing a dang-core module.
typeCheckModule :: Syn.Module -> Dang Core.Module
typeCheckModule m = pass "tc" $
  do tcm <- runTC (tcModule m)
     logInfo (text "Type checking output:" $$ pp tcm)
     return tcm
