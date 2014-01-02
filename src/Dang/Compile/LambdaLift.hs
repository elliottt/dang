
module Dang.Compile.LambdaLift (
    lambdaLift
  ) where

import Dang.Core.AST
import Dang.Monad ( Dang )


-- External Interface ----------------------------------------------------------

lambdaLift :: Module -> Dang Module
lambdaLift  = return
