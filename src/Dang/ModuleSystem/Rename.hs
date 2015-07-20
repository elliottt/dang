{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dang.ModuleSystem.Rename where

import Dang.Monad
import Dang.Syntax.AST
import Dang.Syntax.Location
import Dang.ModuleSystem.Name
import Dang.Utils.PP
import Dang.Utils.Panic (panic)

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           MonadLib (runM,BaseM(..),ReaderT,StateT,get,set,ask,local)


rename :: Namespace -> ModStruct PName -> Dang (ModStruct Name)
rename ns = runRN ns . rnModStruct


-- Monad -----------------------------------------------------------------------

newtype RN a = RN { unRN :: ReaderT RO (StateT RW Dang) a
                  } deriving (Functor,Applicative,Alternative,Monad,MonadPlus)

instance BaseM RN Dang where
  inBase m = RN (inBase m)


runRN :: Namespace -> RN a -> Dang a
runRN ns m = fmap fst $
  runM (unRN m) RO { roNS    = ns     }
                RW { rwNames = mempty }


data RO = RO { roNS :: Namespace
             }

data RW = RW { rwNames :: NameMap
             }

getNamespace :: RN Namespace
getNamespace  = RN (roNS <$> ask)


-- Name Maps -------------------------------------------------------------------

newtype NameMap = NameMap (Map.Map PName [Origin])
                  deriving (Show)

instance Monoid NameMap where
  mempty                          = NameMap mempty
  mappend (NameMap a) (NameMap b) = NameMap (Map.unionWith mappend a b)

data Origin = FromDef Name
              deriving (Show)

ppOrigin :: Origin -> Doc
ppOrigin (FromDef n) = pp n

origName :: Origin -> Name
origName (FromDef n) = n

data NameResult = Resolved Origin
                | Conflict PName [Origin]
                  -- ^ A non-empty list of conflicting names
                | Unknown
                  deriving (Show)

lookupPName :: PName -> NameMap -> NameResult
lookupPName pn (NameMap names) =
  case Map.lookup pn names of

    Just []  -> panic "Dang.Module.Rename:lookupPName"
                      "Invalid naming environment"

    Just [o] -> Resolved o
    Just os  -> Conflict pn os
    Nothing  -> Unknown


-- Renaming --------------------------------------------------------------------

rnLoc :: Located a -> (a -> RN b) -> RN (Located b)
rnLoc Located { .. } f = withLoc locRange $
  do b <- f locValue
     return Located { locValue = b, .. }

-- | Qualify all of the declarations in the struct.
rnModStruct :: ModStruct PName -> RN (ModStruct Name)
rnModStruct (ModStruct ds) =
  do ns <- getNamespace
     undefined

-- | Replace a parsed name with a resolved one.
rnPName :: PName -> RN Name
rnPName pn =
  do RW { .. } <- RN get
     case lookupPName pn rwNames of
       Resolved o    -> return (origName o)
       Conflict n os -> conflict n os
       Unknown       -> unknown pn


-- Errors/Warnings -------------------------------------------------------------

conflict :: PName -> [Origin] -> RN Name
conflict n os =
  do addError (vcat (msg : map ppOrigin os))
     return (origName (head os))
  where
  msg = text "the symbol"
    <+> pp n
    <+> text "is defined in multiple places:"

-- | Invent a name for a parsed name, and record an error about a missing
-- identifier.
unknown :: PName -> RN Name
unknown pn =
  do addError (text "not in scope:" <+> pp pn)
     mkUnknown pn `fmap` askLoc
