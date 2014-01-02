{-# LANGUAGE DeriveGeneric #-}


module Dang.ModuleSystem.Interface where

import Dang.IO ( withWOBinaryFile )
import Dang.Monad ( Dang, io )
import Dang.ModuleSystem.Export ( Export(..) )
import Dang.ModuleSystem.QualName ( Name, ModName, moduleIface )

import           Control.Applicative ( (<|>) )
import qualified Data.ByteString as B
import           Data.Monoid ( Monoid(..) )
import           Data.Serialize ( Serialize(..), decode, encode )
import           GHC.Generics ( Generic )
import qualified Data.Map as Map


-- Querying --------------------------------------------------------------------

class HasIface iface where
  lookupDef :: Name -> iface -> Maybe IfaceDef


-- Interfaces ------------------------------------------------------------------

data Iface = Iface { ifaceModName :: ModName
                   , ifaceDefs    :: Map.Map Name IfaceDef
                   } deriving (Show,Generic)

data IfaceDef = IfaceDefBind IfaceBind
                deriving (Show,Generic)


data IfaceBind = IfaceBind { ibName   :: Name
                           , ibExport :: Export
                           } deriving (Show,Generic)


instance Serialize Iface
instance Serialize IfaceDef
instance Serialize IfaceBind

writeIface :: Iface -> Dang ()
writeIface iface =
  withWOBinaryFile (moduleIface (ifaceModName iface))
    (\ h -> io (B.hPutStr h (encode iface)))


-- Interface Sets --------------------------------------------------------------

data IfaceSet = IfaceSet { isChildren :: Map.Map String IfaceSet
                         , isIface    :: Maybe Iface
                         } deriving (Show)

instance Monoid IfaceSet where
  mempty      = IfaceSet { isChildren = mempty, isIface = Nothing }
  mappend l r = IfaceSet { isChildren = Map.union (isChildren l) (isChildren r)
                         , isIface    = isIface l <|> isIface r }
