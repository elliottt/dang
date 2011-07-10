{-# LANGUAGE DeriveDataTypeable #-}

module QualName where

import Pretty
import Utils (splitLast)

import Control.Applicative ((<$>),(<*>))
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Serialize (Get,Putter,Serialize(get,put),getWord8,putWord8)
import Data.Typeable (Typeable)
import Numeric (showHex)


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
  get = getQualName
  put = putQualName

getQualName :: Get QualName
getQualName  = getWord8 >>= \tag ->
  case tag of
    0 -> QualName <$> get <*> get
    1 -> PrimName <$> get
    _ -> fail ("QualName: unknown tag 0x" ++ showHex tag "")

putQualName :: Putter QualName
putQualName (QualName ps n) = putWord8 0 >> put ps >> put n
putQualName (PrimName n)    = putWord8 1 >> put n

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

-- | Get the module name associated with a qualified name.
qualModule :: QualName -> Maybe QualName
qualModule qn = do
  let pfx = qualPrefix qn
  guard (not (null pfx))
  (ns,n) <- splitLast pfx
  return (QualName ns n)

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
