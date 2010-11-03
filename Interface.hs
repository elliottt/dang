module Interface where

import AST
import LLVM

import qualified Data.Map as Map


newtype Symbol = Symbol String

data Interface = Interface
  { intSymbols :: Map.Map Var Symbol
  }

emptyInterface :: Interface
emptyInterface  = Interface
  { intSymbols = Map.empty
  }

addSymbol :: Var -> Symbol -> Interface -> Interface
addSymbol n s i = i
  { intSymbols = Map.insert n s (intSymbols i)
  }

findSymbol :: Var -> Interface -> Maybe Symbol
findSymbol n = Map.lookup n . intSymbols
