module TypeChecker.Env where

import QualName (QualName)
import TypeChecker.AST (Term)

import qualified Data.Map as Map

type Assumps ty = Map.Map QualName (Assump ty)

data Assump ty = Assump
  { aBody :: Maybe Term
  , aData :: ty
  } deriving (Show)

emptyAssumps :: Assumps ty
emptyAssumps  = Map.empty

lookupAssump :: QualName -> Assumps ty -> Maybe (Assump ty)
lookupAssump  = Map.lookup

addAssump :: QualName -> Assump ty -> Assumps ty -> Assumps ty
addAssump  = Map.insert
