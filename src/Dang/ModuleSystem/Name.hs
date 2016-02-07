{-# LANGUAGE RecordWildCards #-}

module Dang.ModuleSystem.Name (
    ModInfo(..),
    NameSort(..),
    Name(),
    nameSort,
    nameIdent,
    nameUnique,

    mkModName,
    mkBinding,
    mkParam,
    mkUnknown,

    ppNameOrigin,
  ) where

import Dang.Syntax.AST (PName(..))
import Dang.Syntax.Location (HasLoc(..),Range)
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

              | Parameter !Name
                -- ^ Type/function parameter to this declaration.

              | ModDecl !(Maybe ModInfo)
                -- ^ A module, declared in this module.
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
  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}

instance Ord Name where
  compare = compare `on` nUnique
  {-# INLINE compare #-}

instance HasLoc Name where
  getLoc Name { .. } = nFrom
  {-# INLINE getLoc #-}

-- | Retrieve the text associated with the 'Name'.
nameIdent :: Name -> Ident
nameIdent Name { .. } = nName

-- | Information about what kind of 'Name' this is.
nameSort :: Name -> NameSort
nameSort Name { .. } = nSort

-- | Retrieve the unique associated with this name.
nameUnique :: Name -> Unique Name
nameUnique Name { .. } = nUnique


-- Name Construction -----------------------------------------------------------

mkModName :: Maybe [L.Text] -> L.Text -> Range -> Supply -> (Supply,Name)
mkModName mbNs n nFrom s =
  let (s',nUnique) = nextUnique s
      name         = Name { nSort = ModDecl ((ModInfo . packNamespaceLazy) `fmap` mbNs)
                          , nName = mkIdent (L.toStrict n)
                          , .. }
   in (s',name)

-- | Generate a name for a binding site.
mkBinding :: Namespace -> L.Text -> Range -> Supply -> (Supply,Name)
mkBinding ns n nFrom s =
  let (s',nUnique) = nextUnique s
      name         = Name { nSort = Declaration (ModInfo ns)
                          , nName = mkIdent (L.toStrict n)
                          , .. }
   in (s',name)


mkParam :: Name -> L.Text -> Range -> Supply -> (Supply,Name)
mkParam d n nFrom s =
  let (s',nUnique) = nextUnique s
      name         = Name { nSort = Parameter d
                          , nName = mkIdent (L.toStrict n)
                          , .. }
   in (s',name)


-- | Generate a bogus name from a parsed name. This is useful during renaming
-- when we need to generate a name to finish the pass, but have already
-- generated errors, invalidating the output.
mkUnknown :: NameSort -> PName -> Range -> Supply -> (Supply,Name)

mkUnknown nSort (PUnqual n) src s =
  let (s',nUnique) = nextUnique s
      name         = Name { nName = mkIdent (L.toStrict n)
                          , nFrom = src
                          , .. }
   in name `seq` s' `seq` (s',name)

mkUnknown nSort (PQual _ n) src s =
  let (s',nUnique) = nextUnique s
      name         = Name { nName = mkIdent (L.toStrict n)
                          , nFrom = src
                          , .. }
   in name `seq` s' `seq` (s',name)
{-# INLINE mkUnknown #-}


-- Pretty-printing -------------------------------------------------------------

ppNameOrigin :: Name -> Doc
ppNameOrigin Name { .. } =
  case nSort of
    Declaration (ModInfo ns) ->
      text "from module" <+> quotes (pp ns) <+> text "at" <+> pp nFrom

    Parameter fn ->
      text "parameter to" <+> quotes (pp fn) <+> text "at" <+> pp nFrom

    ModDecl (Just (ModInfo ns)) ->
      text "from module" <+> quotes (pp ns) <+> text "at" <+> pp nFrom

    ModDecl Nothing ->
      text "at" <+> pp nFrom


instance PP Name where
  ppr Name { .. } =
    case nSort of

      Declaration (ModInfo ns) ->
        do mb <- getNameFormat ns nName
           case mb of
             Just (Qualified ns') -> pp ns' <> char '.' <> pp nName
             Just UnQualified     ->                       pp nName
             Nothing              -> pp ns  <> char '.' <> pp nName

      ModDecl (Just (ModInfo ns)) ->
        do mb <- getNameFormat ns nName
           case mb of
             Just (Qualified ns') -> pp ns' <> char '.' <> pp nName
             Just UnQualified     ->                       pp nName
             Nothing              -> pp ns  <> char '.' <> pp nName

      ModDecl Nothing ->
        pp nName

      Parameter _ ->
        pp nName
