module ModuleSystem.Export where

import Pretty (Pretty(..),text)


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
