module QualName where

type Name = String

data QualName
  = QualName [Name] Name
  | PrimName Name
    deriving (Eq,Show)

-- | Make a simple name.
simpleName :: Name -> QualName
simpleName  = QualName []

-- | Make a primitive name.
primName :: Name -> QualName
primName  = PrimName
