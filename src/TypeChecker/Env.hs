module TypeChecker.Env where

import QualName (QualName)
import TypeChecker.AST (Term)
import TypeChecker.Types (Scheme)

import qualified Data.Map as Map

type Assumps = Map.Map QualName Assump

data Assump = Assump
  { aBody   :: Maybe Term
  , aScheme :: Scheme
  } deriving (Show)

emptyAssumps :: Assumps
emptyAssumps  = Map.empty

lookupAssump :: QualName -> Assumps -> Maybe Assump
lookupAssump  = Map.lookup

addAssump :: QualName -> Assump -> Assumps -> Assumps
addAssump  = Map.insert
