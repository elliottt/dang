{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Dang.ModuleSystem.Name (
    ModInfo(..),
    ParamSource(..),
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
import Dang.Syntax.Location (HasRange(..),SourceRange)
import Dang.Unique
import Dang.Utils.Ident
import Dang.Utils.PP

import           Data.Function (on)
import qualified Data.Text as T


data ModInfo = ModInfo { modName :: !Namespace
                       } deriving (Eq,Show)

data ParamSource = FromBind !Name
                 | FromSig !Name
                 | FromFunctor !Name
                 | FromLambda !SourceRange
                 | FromCase !SourceRange
                   deriving (Eq,Show)

-- | Information about where a name comes from, like in GHC.
data NameSort = Declaration !ModInfo
                -- ^ Externally visible, comes from this module

              | Parameter !ParamSource
                -- ^ Type/function parameter to this declaration.

              | ModDecl !(Maybe ModInfo)
                -- ^ A module, declared in this module.
                deriving (Eq,Show)

data Name = Name { nUnique :: {-# UNPACK #-} !(Unique Name)
                   -- ^ The unique number assigned to this name for this run of
                   -- the compiler.

                 , nSort :: !NameSort
                   -- ^ What kind of name this is.

                 , nName :: {-# UNPACK #-} !Ident
                   -- ^ The actual name.

                 , nFrom :: !SourceRange
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

instance HasRange Name where
  range Name { .. } = nFrom
  {-# INLINE range #-}

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

mkModName :: Maybe Namespace -> T.Text -> SourceRange -> Supply -> (Supply,Name)
mkModName mbNs n nFrom s =
  let (s',nUnique) = nextUnique s
      name         = Name { nSort = ModDecl (ModInfo `fmap` mbNs)
                          , nName = mkIdent n
                          , .. }
   in (s',name)

-- | Generate a name for a binding site.
mkBinding :: Namespace -> T.Text -> SourceRange -> Supply -> (Supply,Name)
mkBinding ns n nFrom s =
  let (s',nUnique) = nextUnique s
      name         = Name { nSort = Declaration (ModInfo ns)
                          , nName = mkIdent n
                          , .. }
   in (s',name)


mkParam :: ParamSource -> T.Text -> SourceRange -> Supply -> (Supply,Name)
mkParam d n nFrom s =
  let (s',nUnique) = nextUnique s
      name         = Name { nSort = Parameter d
                          , nName = mkIdent n
                          , .. }
   in (s',name)


-- | Generate a bogus name from a parsed name. This is useful during renaming
-- when we need to generate a name to finish the pass, but have already
-- generated errors, invalidating the output.
mkUnknown :: NameSort -> PName -> SourceRange -> Supply -> (Supply,Name)

mkUnknown nSort (PUnqual _ n) src s =
  let (s',nUnique) = nextUnique s
      name         = Name { nName = mkIdent n
                          , nFrom = src
                          , .. }
   in name `seq` s' `seq` (s',name)

mkUnknown nSort (PQual _ _ n) src s =
  let (s',nUnique) = nextUnique s
      name         = Name { nName = mkIdent n
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

    Parameter (FromBind fn) ->
      text "parameter to" <+> quotes (pp fn) <+> text "at" <+> pp nFrom

    Parameter (FromSig sig) ->
      text "type parameter to" <+> quotes (pp sig) <+> text "at" <+> pp nFrom

    Parameter (FromFunctor f) ->
      text "parameter to functor" <+> quotes (pp f) <+> text "at" <+> pp nFrom

    Parameter FromLambda{} ->
      text "parameter to lambda abstraction at" <+> pp nFrom

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
