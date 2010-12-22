module Interface where

import QualName

import Data.Int (Int32)
import qualified Data.Map as Map


data FunDecl = FunDecl
  { funSymbol :: String
  , funArity  :: Int32
  } deriving Show

data Interface = Interface
  { intFunDecls :: Map.Map QualName FunDecl
  }

emptyInterface :: Interface
emptyInterface  = Interface
  { intFunDecls = Map.empty
  }

addFunDecl :: QualName -> FunDecl -> Interface -> Interface
addFunDecl n s i = i
  { intFunDecls = Map.insert n s (intFunDecls i)
  }

findFunDecl :: QualName -> Interface -> Maybe FunDecl
findFunDecl n = Map.lookup n . intFunDecls
