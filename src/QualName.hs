{-# LANGUAGE DeriveDataTypeable #-}

module QualName where

import Pretty

import Control.Monad (ap)
import Data.Char (isSpace)
import Data.Serialize (Serialize(get,put),getWord8,putWord8)
import Data.Typeable (Typeable)
import Numeric (showHex)
import qualified Data.Set as Set


type Name = String

type Namespace = [String]

data QualName
  = QualName Namespace Name
  | PrimName Name
    deriving (Ord,Eq,Show,Typeable)

instance Pretty QualName where
  pp _ (QualName ps n) = hcat (map (\p -> text p <> char '.') ps) <> text n
  pp _ (PrimName n)    = text n

instance Serialize QualName where
  get = getWord8 >>= \tag ->
    case tag of
      0 -> QualName `fmap` get `ap` get
      1 -> PrimName `fmap` get
      _ -> fail ("QualName: unknown tag 0x" ++ showHex tag "")

  put (QualName ps n) = putWord8 0 >> put ps >> put n
  put (PrimName n)    = putWord8 1 >> put n

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

-- | Mangle a qualified name into one that is suitable for code generation.
mangle :: QualName -> String
mangle qn = foldr prefix (qualSymbol qn) (qualPrefix qn)
  where
  prefix pfx rest = rename pfx ++ "_" ++ rest
  rename          = concatMap $ \c ->
    case c of
      '_'           -> "__"
      '.'           -> "_"
      _ | isSpace c -> []
        | otherwise -> [c]

qualNamespace :: QualName -> Namespace
qualNamespace (QualName ps n) = ps ++ [n]
qualNamespace (PrimName _)    = []

changeNamespace :: Namespace -> QualName -> QualName
changeNamespace ps (QualName _ n) = QualName ps n
changeNamespace _  qn@PrimName{}  = qn

class Names a where
  identifiers :: a -> Set.Set QualName

instance Names a => Names (Maybe a) where
  identifiers = maybe Set.empty identifiers

instance Names a => Names [a] where
  identifiers = Set.unions . map identifiers
