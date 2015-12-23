{-# LANGUAGE RecordWildCards #-}

module Dang.ModuleSystem.Name (
    ModInfo(..),
    NameSort(..),
    Name(), Namespace,
    nameSource,
    nameSort,
    nameIdent,
    nameUnique,

    mkUnknown,
  ) where

import Dang.Syntax.AST (PName(..))
import Dang.Syntax.Location (Range)
import Dang.Unique
import Dang.Utils.Ident
import Dang.Utils.PP

import           Data.Function (on)
import qualified Data.Text.Lazy as L


data ModInfo = ModInfo { modName :: !Namespace
                       } deriving (Eq,Ord,Show)

-- | Information about where a name comes from, like in GHC.
data NameSort = Declaration !ModInfo
                -- ^ Externally visible, comes from this module

              | Parameter
                -- ^ Type/function parameter.
                deriving (Eq,Ord,Show)

data Name = Name { nUnique :: {-# UNPACK #-} !(Unique Name)
                   -- ^ The unique number assigned to this name for this run of
                   -- the compiler.

                 , nSort :: !NameSort
                   -- ^ What kind of name this is.

                 , nName :: {-# UNPACK #-} !Ident
                   -- ^ The actual name.

                 , nFrom :: {-# UNPACK #-} !Range
                   -- ^ Where this name is defined.
                 } deriving (Show)

instance Eq Name where
  (==) = (==) `on` nUnique
  (/=) = (/=) `on` nUnique 


-- | Retrieve the text associated with the 'Name'.
nameIdent :: Name -> Ident
nameIdent Name { .. } = nName

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
                          , nName = mkIdent (L.toStrict n)
                          , nFrom = src
                          , .. }
   in name `seq` s' `seq` (s',name)

mkUnknown (PQual ns n) src s =
  let (s',nUnique) = nextUnique s
      name         = Name { nSort = Declaration (ModInfo (L.toStrict ns))
                          , nName = mkIdent (L.toStrict n)
                          , nFrom = src
                          , .. }
   in name `seq` s' `seq` (s',name)
{-# INLINE mkUnknown #-}


-- Pretty-printing -------------------------------------------------------------

-- XXX fix this to interact with the environment, asking how to print this
-- particular name.
instance PP Name where
  ppr Name { .. } =
    case nSort of

      Declaration (ModInfo ns) ->
        do mb <- getNameFormat ns nName
           case mb of
             Just (Qualified ns') -> pp ns' <> char '.' <> pp nName
             Just UnQualified     ->                       pp nName
             Nothing              -> pp ns  <> char '.' <> pp nName

      Parameter ->
        pp nName
