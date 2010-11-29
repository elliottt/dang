module Interface where

import Data.Int (Int32)
import Text.LLVM
import qualified Data.Map as Map


data FunDecl = FunDecl
  { funSymbol :: String
  , funArity  :: Int32
  } deriving Show

data Interface = Interface
  { intFunDecls :: Map.Map String FunDecl
  }

emptyInterface :: Interface
emptyInterface  = Interface
  { intFunDecls = Map.empty
  }

addFunDecl :: String -> FunDecl -> Interface -> Interface
addFunDecl n s i = i
  { intFunDecls = Map.insert n s (intFunDecls i)
  }

findFunDecl :: String -> Interface -> Maybe FunDecl
findFunDecl n = Map.lookup n . intFunDecls
