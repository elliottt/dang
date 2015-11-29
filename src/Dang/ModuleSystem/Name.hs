{-# LANGUAGE RecordWildCards #-}

module Dang.ModuleSystem.Name (
    ModInfo(..),
    NameSort(..),
    Name(), Namespace,
    nameSource,
    nameSort,

    mkUnknown,
  ) where

import Dang.Syntax.AST (PName(..))
import Dang.Syntax.Location (Source,Range)
import Dang.Unique
import Dang.Utils.PP

import           Data.Function (on)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L


data ModInfo = ModInfo { modName :: !Namespace
                       } deriving (Eq,Ord,Show)

-- | Information about where a name comes from, like in GHC.
data NameSort = Declaration !ModInfo
                -- ^ Externally visible, comes from this module

              | Parameter
                -- ^ Type/function parameter.
                deriving (Eq,Ord,Show)

type Namespace = S.Text

data Name = Name { nUnique :: {-# UNPACK #-} !(Unique Name)
                   -- ^ The unique number assigned to this name for this run of
                   -- the compiler.

                 , nSort :: !NameSort
                   -- ^ What kind of name this is.

                 , nName :: {-# UNPACK #-} !S.Text
                   -- ^ The actual name.

                 , nFrom :: {-# UNPACK #-} !Range
                   -- ^ Where this name is defined.
                 } deriving (Show)

instance Eq Name where
  (==) = (==) `on` nUnique
  (/=) = (/=) `on` nUnique 


-- | Retrieve the text associated with the 'Name'.
nameString :: Name -> S.Text
nameString Name { .. } = nName

-- | The definition site of the 'Name'.
nameSource :: Name -> Range
nameSource Name { .. } = nFrom

-- | Information about what kind of 'Name' this is.
nameSort :: Name -> NameSort
nameSort Name { .. } = nSort

-- | Retrieve the unique associated with this name.
nameUnique :: Name -> Unique Name
nameUnique Name { .. } = nUnique


-- Name Construction -----------------------------------------------------------

-- | Generate a bogus name from a parsed name. This is useful during renaming
-- when we need to generate a name to finish the pass, but have already
-- generated errors, invalidating the output.
mkUnknown :: PName -> Range -> Supply -> (Supply,Name)

mkUnknown (PUnqual n) src s =
  let (s',nUnique) = nextUnique s
      name         = Name { nSort = Parameter
                          , nName = L.toStrict n
                          , nFrom = src
                          , .. }
   in name `seq` s' `seq` (s',name)

mkUnknown (PQual ns n) src s =
  let (s',nUnique) = nextUnique s
      name         = Name { nSort = Declaration (ModInfo (L.toStrict ns))
                          , nName = L.toStrict n
                          , nFrom = src
                          , .. }
   in name `seq` s' `seq` (s',name)
{-# INLINE mkUnknown #-}


-- Pretty-printing -------------------------------------------------------------

-- XXX fix this to interact with the environment, asking how to print this
-- particular name.
instance PP Name where
  ppr Name { .. } = pp nName
