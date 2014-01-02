{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Dang.ModuleSystem.Export where

import Dang.Traversal (Data,Typeable)
import Dang.Utils.Location (Located,unLoc)
import Dang.Utils.Pretty (Pretty(..),PPDoc,text,hang)

import Data.Function ( on )
import Data.List ( groupBy )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )


-- Export Specifications -------------------------------------------------------

data Export = Public | Private
    deriving (Eq,Show,Ord,Data,Typeable,Generic)

instance Serialize Export

instance Pretty Export where
  ppr Public  = text "public"
  ppr Private = text "private"

class Exported a where
  exportSpec :: a -> Export

instance Exported Export where
  {-# INLINE exportSpec #-}
  exportSpec = id

instance Exported a => Exported (Located a) where
  {-# INLINE exportSpec #-}
  exportSpec loc = exportSpec (unLoc loc)

isExported :: Exported a => a -> Bool
isExported a = case exportSpec a of
  Public -> True
  _      -> False

groupByExport :: Exported a => [a] -> [[a]]
groupByExport  = groupBy ((==) `on` exportSpec)

ppPublic :: PPDoc -> PPDoc
ppPublic  = hang (text "public") 2

ppPrivate :: PPDoc -> PPDoc
ppPrivate  = hang (text "private") 2
