{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Dang.ModuleSystem.Rename (
    rename,
    renameModule,

    rnLoc,
    rnModStruct,
    rnModBind,
    rnSchema,
    rnMatch,
    rnDecl,
    rnBind,
  ) where

import Dang.Monad
import Dang.Syntax.AST
import Dang.Syntax.Location
import Dang.ModuleSystem.Name (Name,Namespace,mkUnknown)
import Dang.Unique (withSupply)
import Dang.Utils.PP
import Dang.Utils.Panic (panic)

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           MonadLib (runM,BaseM(..),ReaderT,StateT,get,set,ask,local)


renameModule :: Module PName -> Dang (Module Name)
renameModule Module { .. } = rename modName $
  do ds <- traverse (rnLoc rnDecl) modDecls
     return Module { modDecls = ds, .. }


-- Monad -----------------------------------------------------------------------

newtype RN a = RN { unRN :: ReaderT RO (StateT RW Dang) a
                  } deriving (Functor,Applicative,Alternative,Monad,MonadPlus)

instance BaseM RN Dang where
  inBase m = RN (inBase m)


rename :: Namespace -> RN a -> Dang a
rename ns m = fmap fst $
  runM (unRN m) RO { roNS    = ns     }
                RW { rwNames = mempty }


data RO = RO { roNS :: Namespace
             }

data RW = RW { rwNames :: NameMap
             }

getNamespace :: RN Namespace
getNamespace  = RN (roNS <$> ask)


-- Name Maps -------------------------------------------------------------------

newtype NameMap = NameMap (Map.Map Def [Name])
                  deriving (Show)

data Def = DefDecl PName
         | DefType PName
           deriving (Eq,Ord,Show)

instance PP Def where
  ppr (DefDecl n) = ppr n
  ppr (DefType n) = ppr n

defName :: Def -> PName
defName (DefDecl n) = n
defName (DefType n) = n

defType :: Def -> Doc
defType DefDecl{} = text "value"
defType DefType{} = text "type"

instance Monoid NameMap where
  mempty                          = NameMap mempty
  mappend (NameMap a) (NameMap b) = NameMap (Map.unionWith merge a b)
    where
    merge as bs | as == bs  = as
                | otherwise = as ++ bs

data NameResult = Resolved Name
                | Conflict Def [Name]
                  -- ^ A non-empty list of conflicting names
                | Unknown
                  deriving (Show)

lookupDef :: Def -> NameMap -> NameResult
lookupDef d (NameMap names) =
  case Map.lookup d names of

    Just []  -> panic "Dang.Module.Rename:lookupPName"
                      ("Invalid naming environment" :: String)

    Just [n] -> Resolved n
    Just ns  -> Conflict d ns
    Nothing  -> Unknown


-- Renaming --------------------------------------------------------------------

type Rename f = f PName -> RN (f Name)

rnLoc :: (a -> RN b) -> Located a -> RN (Located b)
rnLoc f Located { .. } = withLoc locRange $
  do b <- f locValue
     return Located { locValue = b, .. }

-- | Qualify all of the declarations in the struct.
rnModStruct :: Rename ModStruct
rnModStruct (ModStruct ds) =
  do ns <- getNamespace
     undefined

-- | Replace a parsed name with a resolved one.
rnPName :: Def -> RN Name
rnPName d =
  do RW { .. } <- RN get
     case lookupDef d rwNames of
       Resolved n    -> return n
       Conflict n os -> conflict n os
       Unknown       -> unknown d

-- | Rename a declaration.
rnDecl :: Rename Decl
rnDecl (DBind b)     = DBind    <$> rnBind b
rnDecl (DModBind mb) = DModBind <$> rnModBind mb
rnDecl (DLoc d)      = DLoc     <$> rnLoc rnDecl d
rnDecl (DSig s)      = panic "rename" $ text "Unexpected signature found"
                                     $$ text (show s)

-- | Rename a binding.
rnBind :: Rename Bind
rnBind Bind { .. } =
  do n'  <- rnLoc (rnPName . DefDecl) bName
     mb' <- traverse rnSchema bSchema
     b'  <- rnMatch bBody
     return Bind { bName = n', bSchema = mb', bBody = b' }

-- | Rename a module binding.
rnModBind :: Rename ModBind
rnModBind ModBind { .. } = undefined


-- Expressions -----------------------------------------------------------------

rnMatch :: Rename Match
rnMatch  = undefined


-- Types -----------------------------------------------------------------------

rnSchema :: Rename Schema
rnSchema  = undefined


-- Errors/Warnings -------------------------------------------------------------

conflict :: Def -> [Name] -> RN Name
conflict d ns =
  do addError (vcat (msg : map ppr ns))
     return (head ns)
  where
  msg = text "the"
    <+> defType d
    <+> pp d
    <+> text "is defined in multiple places:"

-- | Invent a name for a parsed name, and record an error about a missing
-- identifier.
unknown :: Def -> RN Name
unknown d =
  do addError (text "not in scope:" <+> pp d)
     loc <- askLoc
     inBase (withSupply (mkUnknown (defName d) loc))
