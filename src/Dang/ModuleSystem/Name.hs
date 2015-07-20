{-# LANGUAGE RecordWildCards #-}

module Dang.ModuleSystem.Name where

import           Dang.Syntax.AST (PName(..))
import           Dang.Syntax.Location (Source,Range)
import           Dang.Utils.PP

import qualified Data.Text as T


type Namespace = [T.Text]

data ModInfo = ModInfo { modName :: Namespace
                       } deriving (Eq,Ord,Show)

-- | Information about where a name comes from, like in GHC.
data NameSort = External !ModInfo
                -- ^ Externally visible, comes from this module
              | Internal
                -- ^ Internally defined (parameter, type var, etc)
                deriving (Eq,Ord,Show)

data Name = Name { nSort :: !NameSort
                 , nName :: !T.Text
                 , nFrom :: !Range
                 } deriving (Eq,Ord,Show)


nameSource :: Name -> Range
nameSource Name { .. } = nFrom

nameSort :: Name -> NameSort
nameSort Name { .. } = nSort

isPublic :: Name -> Bool
isPublic n =
  case nameSort n of
    External _ -> True
    _          -> False


-- Name Construction -----------------------------------------------------------

-- | Generate a bogus name from a parsed name.
mkUnknown :: PName -> Range -> Name

mkUnknown (PUnqual n) src =
  Name { nSort = Internal
       , nName = n
       , nFrom = src }

mkUnknown (PQual _ n) src =
  Name { nSort = Internal
       , nName = n
       , nFrom = src }


-- Pretty-printing -------------------------------------------------------------

-- XXX fix this to interact with the environment, asking how to print this
-- particular name.
instance PP Name where
  ppr Name { .. } = pp nName
