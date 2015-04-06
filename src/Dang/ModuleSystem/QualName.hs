{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Dang.ModuleSystem.QualName where

import Dang.Utils.Pretty

import Control.Lens ( Lens', lens, view, set )
import Data.Char (isSpace)
import Data.Data ( Data )
import Data.Function (on)
import Data.Serialize ( Serialize )
import Data.String ( IsString(..) )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import System.FilePath ( joinPath, (<.>) )


-- Levels ----------------------------------------------------------------------

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



-- Namespaces ------------------------------------------------------------------

type ModName = [String]

moduleIface :: ModName -> FilePath
moduleIface m = joinPath m <.> "di"

ppModName :: ModName -> PPDoc
ppModName mn = hcat (punctuate (char '.') (map text mn))


-- Qualified Names -------------------------------------------------------------

-- | A fully-qualified name, referring to either a parameter, or a declared
-- name.
data QualName = Qual Level ModName String
                deriving (Show,Eq,Ord,Data,Typeable,Generic)

unqual :: QualName -> QualName
unqual  = set qualModule []

instance Serialize QualName

instance Pretty QualName where
  ppr (Qual l ns n) = dots (map text (ns ++ [n])) <> pp l


-- | The level from a qualified name.
qualLevel :: Lens' QualName Level
qualLevel  = lens getter setter
  where
  getter (Qual l _ _)    = l
  setter (Qual _ ns n) l = Qual l ns n

-- | Get the module name associated with a name.
qualModule :: Lens' QualName ModName
qualModule  = lens getter setter
  where
  getter (Qual _ m _)   = m
  setter (Qual l _ n) m = Qual l m n

-- | Get the name part of the qualified name.
qualSymbol :: Lens' QualName String
qualSymbol  = lens getter setter
  where
  getter (Qual _ _ n)    = n
  setter (Qual l ns _) n = Qual l ns n

-- | Mangle a qualified name into one that is suitable for code generation.
mangle :: IsString string => QualName -> string
mangle name = fromString (foldr prefix (view qualSymbol name) modName)
  where
  modName         = view qualModule name
  prefix pfx rest = escape pfx ++ "_" ++ rest
  escape          = concatMap $ \c ->
    case c of
      '_'           -> "__"
      '.'           -> "_"
      _ | isSpace c -> []
        | otherwise -> [c]


-- Names -----------------------------------------------------------------------

data Name = Parsed String QualName
          | Generated QualName
            deriving (Show,Data,Typeable,Generic)

instance Eq Name where
  (==) = (==) `on` view qualName
  (/=) = (/=) `on` view qualName

instance Ord Name where
  compare = compare `on` view qualName

instance Serialize Name

instance Pretty Name where
  ppr name = case name of
    Parsed n qn  -> do printQual <- getPrintQual
                       if printQual
                          then ppr qn
                          else text n
    Generated qn -> ppr qn

qualName :: Lens' Name QualName
qualName  = lens getter setter
  where
  getter (Parsed _ qn)  = qn
  getter (Generated qn) = qn

  setter (Parsed n _)  qn = Parsed n qn
  setter (Generated _) qn = Generated qn

mkQual :: Level -> ModName -> String -> Name
mkQual l ns n = Generated (Qual l ns n)
