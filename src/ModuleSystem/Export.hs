module ModuleSystem.Export where

import Pretty (Pretty(..),Doc,text,isEmpty,empty,nest,($$))


-- Export Specifications -------------------------------------------------------

data Export = Public | Private
    deriving (Eq,Show,Ord)

instance Pretty Export where
  pp _ Public  = text "public"
  pp _ Private = text "private"

class Exported a where
  exportSpec :: a -> Export

isExported :: Exported a => a -> Bool
isExported a = case exportSpec a of
  Public -> True
  _      -> False

ppPublic :: Doc -> Doc
ppPublic d | isEmpty d = empty
           | otherwise = text "public" $$ nest 2 d

ppPrivate :: Doc -> Doc
ppPrivate d | isEmpty d = empty
            | otherwise = text "private" $$ nest 2 d
