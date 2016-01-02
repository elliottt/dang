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
import Dang.ModuleSystem.Name (Name,mkUnknown,mkBinding)
import Dang.Unique (SupplyM,withSupply)
import Dang.Utils.Ident (Namespace)
import Dang.Utils.PP
import Dang.Utils.Panic (panic)

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus)
import qualified Data.Foldable as F
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           MonadLib (runM,BaseM(..),ReaderT,ask,local)


renameModule :: Module PName -> Dang (Module Name)
renameModule Module { .. } = rename (thing modName) $
  do declEnv <- mergeNames declNames modDecls
     withNames declEnv $
       do ds <- traverse rnDecl modDecls
          return Module { modDecls = ds, .. }


-- Monad -----------------------------------------------------------------------

newtype RN a = RN { unRN :: ReaderT RO Dang a
                  } deriving (Functor,Applicative,Alternative,Monad,MonadPlus)

instance BaseM RN Dang where
  inBase m = RN (inBase m)

instance SupplyM RN where
  withSupply f = inBase (withSupply f)


rename :: Namespace -> RN a -> Dang a
rename ns m = runM (unRN m) RO { roNS = ns, roNames = mempty }


data RO = RO { roNS    :: Namespace
             , roNames :: NameMap
             }

-- | Extend the current namespace with the given one.
pushNamespace :: Namespace -> RN a -> RN a
pushNamespace ns m =
  do ro <- RN ask
     let ns' = roNS ro `T.append` "." `T.append` ns
     RN (local ro { roNS = ns' } (unRN m))

getNamespace :: RN Namespace
getNamespace  = RN (roNS <$> ask)

withNames :: NameMap -> RN a -> RN a
withNames names m =
  do ro     <- RN ask
     names' <- checkShadowing names (roNames ro)
     RN (local ro { roNames = names' } (unRN m))


-- Name Maps -------------------------------------------------------------------

newtype NameMap = NameMap (Map.Map Def [Name])
                  deriving (Show)

instance Monoid NameMap where
  mempty                          = NameMap mempty
  mappend (NameMap a) (NameMap b) = NameMap (Map.unionWith merge a b)
    where
    merge as bs | as == bs  = as
                | otherwise = as ++ bs

singleton :: Def -> Name -> NameMap
singleton d n = NameMap (Map.singleton d [n])


-- | Merge name mappings, but add errors when overlap occurs.
checkMerge :: NameMap -> NameMap -> RN NameMap
checkMerge l@(NameMap xs) r@(NameMap ys) =
  do Map.traverseWithKey conflict (Map.intersectionWith nubMerge xs ys)
     return (l `mappend` r)

  where
  nubMerge as bs = nub (as ++ bs)


checkShadowing :: NameMap -> NameMap -> RN NameMap
checkShadowing l@(NameMap xs) r@(NameMap ys) =
  do Map.traverseWithKey shadows (Map.intersectionWith (,) xs ys)
     return (l `mappend` r)


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


type GetNames f = f PName -> RN NameMap

mergeNames :: Foldable f => (a -> RN NameMap) -> f a -> RN NameMap
mergeNames f = F.foldlM step mempty
  where
  step acc a =
    do nm <- f a
       -- XXX this should collect conflicts
       return (nm `mappend` acc)


-- | Introduce names for the given binding.
bindName :: GetNames Bind
bindName Bind { bName = Located { .. }, .. } =
  case locValue of
    -- there should only ever be unqualified names in bindings
    PUnqual t -> do ns   <- getNamespace
                    name <- withSupply (mkBinding ns t locRange)
                    return (singleton (DefDecl locValue) name)

    PQual{} -> panic "renamer" (text "Qualified name in binding, parser bug?")


-- | Introduce names for all bindings within a declaration. NOTE: this will
-- traverse into module definitions, introducing names for visible bindings.
declNames :: GetNames Decl
declNames (DBind b)     = bindName b
declNames (DModBind mb) = modBindNames mb
declNames (DLoc dl)     = addLoc dl declNames
declNames DSig{}        = return mempty

modBindNames :: GetNames ModBind
modBindNames ModBind { .. } =
  addLoc mbName (\ns -> pushNamespace ns (modExprNames mbExpr))

modExprNames :: GetNames ModExpr
modExprNames (MEStruct ms)       = modStructNames ms
modExprNames (MEConstraint me _) = modExprNames me
modExprNames (MELoc ml)          = addLoc ml modExprNames
modExprNames _                   = return mempty

modStructNames :: GetNames ModStruct
modStructNames ModStruct { .. } = mergeNames (`addLoc` declNames) msElems


-- Renaming --------------------------------------------------------------------

type Rename f = f PName -> RN (f Name)

rnLoc :: (a -> RN b) -> Located a -> RN (Located b)
rnLoc f Located { .. } = withLoc locRange $
  do b <- f locValue
     return Located { locValue = b, .. }

-- | Replace a parsed name with a resolved one.
rnPName :: Def -> RN Name
rnPName d =
  do RO { .. } <- RN ask
     case lookupDef d roNames of
       Resolved n    -> return n
       Conflict n os -> conflict n os
       Unknown       -> unknown d


-- Modules ---------------------------------------------------------------------

-- | Qualify all of the declarations in the struct.
rnModStruct :: Rename ModStruct
rnModStruct (ModStruct ds) =
  do ns <- getNamespace
     undefined

-- | Rename a declaration.
rnDecl :: Rename Decl
rnDecl (DBind b)     = DBind    <$> rnBind b
rnDecl (DModBind mb) = DModBind <$> rnModBind mb
rnDecl (DLoc d)      = DLoc     <$> rnLoc rnDecl d
rnDecl (DSig s)      = panic "rename" $ text "Unexpected signature found"
                                     $$ text (show s)

-- | Rename a module binding.
rnModBind :: Rename ModBind
rnModBind ModBind { .. } = undefined


-- Expressions -----------------------------------------------------------------

rnMatch :: Rename Match
rnMatch  = undefined

-- | Rename a binding. This assumes that new names have already been introduced
-- externally.
rnBind :: Rename Bind
rnBind Bind { .. } =
  do n'  <- rnLoc (rnPName . DefDecl) bName
     mb' <- traverse rnSchema bSchema
     b'  <- rnMatch bBody
     return Bind { bName = n', bSchema = mb', bBody = b' }


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

shadows :: Def -> ([Name],[Name]) -> RN ()
shadows d (new,old)
  | null new || null old = panic "renamer" (text "Invalid use of `shadows`")
  | otherwise            = addWarning msg
  where
  msg = text "the definition of"
    <+> pp (head new)
    <+> text "shadows the definition of"
    <+> pp (head old)

-- | Invent a name for a parsed name, and record an error about a missing
-- identifier.
unknown :: Def -> RN Name
unknown d =
  do addError (text "not in scope:" <+> pp d)
     loc <- askLoc
     inBase (withSupply (mkUnknown (defName d) loc))
