{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Dang.ModuleSystem.QualName where

import Dang.Utils.Location
import Dang.Utils.Pretty

import Data.Char (isSpace)
import Data.Data ( Data )
import Data.Maybe ( fromMaybe )
import Data.Serialize ( Serialize )
import Data.String ( IsString(..) )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import System.FilePath ( joinPath, (<.>) )


-- | Where a name lives.  This is to allow things at different levels
-- (expression, type, kind) to share the same name.
data Level = Expr | Type Int
             deriving (Show,Eq,Ord,Data,Typeable,Generic)

instance Serialize Level

instance Pretty Level where
  ppr l =
    do cond <- getPrintLevels
       if cond then case l of
                      Expr   -> brackets (char 'e')
                      Type i -> brackets (char 't' <> int i)
               else empty


type ModName = [String]

moduleIface :: ModName -> FilePath
moduleIface m = joinPath m <.> "di"

ppModName :: ModName -> PPDoc
ppModName mn = hcat (punctuate (char '.') (map text mn))

type LName = Located Name

data Name = LocalName Level String
          | QualName Level ModName String
            deriving (Ord,Eq,Show,Data,Typeable,Generic)

instance Serialize Name

instance Pretty Name where
  ppr name = case name of
    LocalName l n   -> text n <> pp l
    QualName l ns n -> dots (map text ns ++ [text n]) <> pp l

mkLocal :: Level -> String -> Name
mkLocal  = LocalName

mkQual :: Level -> ModName -> String -> Name
mkQual  = QualName

nameLevel :: Name -> Level
nameLevel (LocalName l _)  = l
nameLevel (QualName l _ _) = l

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
