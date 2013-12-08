{-# LANGUAGE DeriveDataTypeable #-}

module Dang.ModuleSystem.QualName where

import Dang.Traversal (Data,Typeable)
import Dang.Utils.Location
import Dang.Utils.Pretty

import Control.Applicative ((<$>),(<*>))
import Data.Char (isSpace)
import Data.Maybe ( fromMaybe )
import Data.Serialize (Get,Putter,Serialize(get,put),getWord8,putWord8)
import Data.String ( IsString(..) )
import Numeric (showHex)


-- | Where a name lives.  This is to allow things at different levels
-- (expression, type, kind) to share the same name.
data Level = Expr | Type Int
             deriving (Show,Eq,Ord,Data,Typeable)

instance Pretty Level where
  ppr l =
    do cond <- getPrintLevels
       if cond then case l of
                      Expr   -> brackets (char 'e')
                      Type i -> brackets (char 't' <> int i)
               else empty

putLevel :: Putter Level
putLevel l = case l of
  Expr   -> putWord8 0
  Type i -> putWord8 1 >> put i

getLevel :: Get Level
getLevel  = getWord8 >>= \ tag -> case tag of
  0 -> return Expr
  1 -> Type <$> get
  _ -> fail ("unexpected tag: " ++ show tag)


type ModName = [String]

putModName :: Putter ModName
putModName  = put

getModName :: Get ModName
getModName  = get


type LName = Located Name

data Name = LocalName Level String
          | QualName Level ModName String
            deriving (Ord,Eq,Show,Data,Typeable)

instance Pretty Name where
  ppr name = case name of
    LocalName l n   -> text n <> pp l
    QualName l ns n -> dots (map text ns ++ [text n]) <> pp l

getName :: Get Name
getName  = getWord8 >>= \tag ->
  case tag of
    0 -> LocalName <$> getLevel <*> get
    1 -> QualName  <$> getLevel <*> getModName <*> get
    _ -> fail ("QualName: unknown tag 0x" ++ showHex tag "")

putName :: Putter Name
putName name = case name of
  LocalName l n   -> putWord8 0 >> putLevel l >> put n
  QualName l mn n -> putWord8 0 >> putLevel l >> putModName mn >> put n

mkLocal :: Level -> String -> Name
mkLocal  = LocalName

mkQual :: Level -> ModName -> String -> Name
mkQual  = QualName

-- | Get the name part of a name
qualSymbol :: Name -> String
qualSymbol name = case name of
  LocalName _ n  -> n
  QualName _ _ n -> n

-- | Modify the symbol in a name.
mapSymbol :: (String -> String) -> (Name -> Name)
mapSymbol f name = case name of
  LocalName l n   -> LocalName l (f n)
  QualName l mn n -> QualName l mn (f n)

-- | Get the module name associated with a name.
qualModule :: Name -> Maybe ModName
qualModule name = case name of
  QualName _ m _ -> Just m
  LocalName{}    -> Nothing

-- | Mangle a name into one that is suitable for code generation.
mangle :: IsString string => Name -> string
mangle name = fromString (foldr prefix (qualSymbol name) modName)
  where
  modName         = fromMaybe [] (qualModule name)
  prefix pfx rest = rename pfx ++ "_" ++ rest
  rename          = concatMap $ \c ->
    case c of
      '_'           -> "__"
      '.'           -> "_"
      _ | isSpace c -> []
        | otherwise -> [c]

-- | Modify the namespace of a name.
changeModule :: ModName -> Name -> Name
changeModule m name = case name of
  QualName l _ n -> QualName l m n
  LocalName{}    -> name
