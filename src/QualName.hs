module QualName where

import Pretty


type Name = String

type Namespace = [String]

data QualName
  = QualName Namespace Name
  | PrimName Name
    deriving (Ord,Eq,Show)

instance Pretty QualName where
  pp _ (QualName ps n) = hcat (map (\p -> text p <> char '.') ps) <> text n
  pp _ (PrimName n)    = text n

-- | Make a qualified name.
qualName :: Namespace -> Name -> QualName
qualName  = QualName

-- | Make a simple name.
simpleName :: Name -> QualName
simpleName  = QualName []

isSimpleName :: QualName -> Bool
isSimpleName (QualName ns _) = null ns
isSimpleName _               = False

-- | Make a primitive name.
primName :: Name -> QualName
primName  = PrimName

-- | Get the prefix of a qualified name.
qualPrefix :: QualName -> Namespace
qualPrefix (QualName ps _) = ps
qualPrefix (PrimName _)    = []

-- | Get the name part of a qualified name
qualSymbol :: QualName -> Name
qualSymbol (QualName _ n) = n
qualSymbol (PrimName n)   = n
