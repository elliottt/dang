{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dang.ModuleSystem.QualName where

import Dang.Utils.Pretty

import Control.Lens ( Lens', lens, view, Prism', prism, _2, Traversal', preview )
import Data.Char (isSpace)
import Data.Data ( Data )
import Data.Maybe ( fromMaybe )
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

instance Pretty Level i where
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

ppModName :: ModName -> PPDoc i
ppModName mn = hcat (punctuate (char '.') (map text mn))


-- Qualified Names -------------------------------------------------------------

-- | A fully-qualified name, referring to either a parameter, or a declared
-- name.
data QualName = Param Level String
              | Qual Level ModName String
                deriving (Show,Eq,Ord,Data,Typeable,Generic)

instance Serialize QualName

instance Pretty QualName i where
  ppr (Param l n)   = text n <> pp l
  ppr (Qual l ns n) = dots (map text (ns ++ [n])) <> pp l


-- | The level from a qualified name.
qualLevel :: Lens' QualName Level
qualLevel  = lens getter setter
  where
  getter (Param l _)  = l
  getter (Qual l _ _) = l

  setter (Param _ n)   l = Param l n
  setter (Qual _ ns n) l = Qual l ns n

-- | Get the name part of the qualified name.
qualSymbol :: Lens' QualName String
qualSymbol  = lens getter setter
  where
  getter (Param _ n)  = n
  getter (Qual _ _ n) = n

  setter (Param l _)   n = Param l n
  setter (Qual l ns _) n = Qual l ns n

_qual :: Prism' QualName (Level,ModName,String)
_qual  = prism mk prj
  where
  mk (l,ns,n) = Qual l ns n

  prj (Qual l ns n) = Right (l,ns,n)
  prj qn            = Left qn

_param :: Prism' QualName (Level,String)
_param  = prism mk prj
  where
  mk (l,n)        = Param l n

  prj (Param l n) = Right (l,n)
  prj qn          = Left qn

-- | Get the module name associated with a name.
qualModule :: Traversal' QualName ModName
qualModule  = _qual . _2

-- | Mangle a qualified name into one that is suitable for code generation.
mangle :: IsString string => QualName -> string
mangle name = fromString (foldr prefix (view qualSymbol name) modName)
  where
  modName         = fromMaybe [] (preview qualModule name)
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
            deriving (Ord,Eq,Show,Data,Typeable,Generic)

instance Serialize Name

instance Pretty Name i where
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

mkParam :: Level -> String -> Name
mkParam l n = Generated (Param l n)

mkQual :: Level -> ModName -> String -> Name
mkQual l ns n = Generated (Qual l ns n)
