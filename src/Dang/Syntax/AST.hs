{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}

module Dang.Syntax.AST where

import Dang.AST
import Dang.Syntax.Location
import Dang.Utils.PP

import           Data.List (intersperse)
import qualified Data.Text as T
import           GHC.Generics (Generic)


-- Syntax and Metadata ---------------------------------------------------------

-- | The syntax descriptor for parsed modules.
data Parsed

type instance IdentOf  Parsed = PName
type instance TypeOf   Parsed = Type Parsed
type instance SchemaOf Parsed = Schema Parsed
type instance MetaOf   Parsed = SourceRange


-- AST -------------------------------------------------------------------------

-- | Parsed names, either qualified or unqualified.
data PName = PUnqual !SourceRange !T.Text
           | PQual   !SourceRange ![T.Text] !T.Text
             deriving (Eq,Show,Generic)

instance Ord PName where
  compare (PUnqual _ a) (PUnqual _ b) = compare a b
  compare PUnqual{} _ = LT

  compare (PQual _ as a) (PQual _ bs b) =
    case compare as bs of
      EQ -> compare a b
      x  -> x
  compare PQual{}   _ = GT

pnameNamespace :: PName -> [T.Text]
pnameNamespace (PUnqual _ i)  = [i]
pnameNamespace (PQual _ ns i) = ns ++ [i]

-- | A parsed top-level module.
type PModule = Module Parsed

data Module syn = Module { modMeta     :: MetaOf syn
                         , modName     :: IdentOf syn
                         , modRequires :: [Require syn]
                         , modDecls    :: [Decl syn]
                         } deriving (Generic)

data Require syn = Require { reqMeta   :: MetaOf syn
                           , reqModule :: IdentOf syn
                           , reqOpen   :: Bool
                           }

data ModStruct syn = ModStruct { msMeta  :: MetaOf syn
                               , msElems :: [Decl syn]
                               } deriving (Generic)

data Decl syn = DBind    (MetaOf syn) (Bind syn)
              | DSig     (MetaOf syn) (Sig syn)
              | DData    (MetaOf syn) (Data syn)
              | DModBind (MetaOf syn) (IdentOf syn) (ModExpr syn)
              | DModType (MetaOf syn) (IdentOf syn) (ModType syn)
                deriving (Generic)

data Bind syn = Bind { bMeta   :: MetaOf syn
                     , bName   :: IdentOf syn
                     , bSig    :: Maybe (SchemaOf syn)
                     , bParams :: [Pat syn]
                     , bBody   :: Expr syn
                     } deriving (Generic)

data Sig syn = Sig { sigMeta   :: MetaOf syn
                   , sigName   :: IdentOf syn
                   , sigSchema :: SchemaOf syn
                   } deriving (Generic)

data ModType syn = MTVar     (MetaOf syn) (IdentOf syn)
                 | MTSig     (MetaOf syn) (ModSig syn)
                 | MTFunctor (MetaOf syn) (IdentOf syn) (ModType syn) (ModType syn)
                   -- XXX add with-constraints
                   deriving (Generic)

type ModSig syn = [ModSpec syn]

data ModSpec syn = MSSig  (MetaOf syn) (Sig syn)
                 | MSKind (MetaOf syn) (Sig syn)
                 | MSData (MetaOf syn) (Data syn)
                 | MSMod  (MetaOf syn) (IdentOf syn) (ModType syn)
                   deriving (Generic)

data ModExpr syn = MEName       (MetaOf syn) (IdentOf syn)
                 | MEApp        (MetaOf syn) (ModExpr syn) (ModExpr syn)
                 | MEStruct     (MetaOf syn) (ModStruct syn)
                 | MEFunctor    (MetaOf syn) (IdentOf syn) (ModType syn) (ModExpr syn)
                 | MEConstraint (MetaOf syn) (ModExpr syn) (ModType syn)
                   deriving (Generic)

data Match syn = MPat   (MetaOf syn) (Pat syn) (Match syn)
               | MSplit (MetaOf syn) (Match syn) (Match syn)
               | MFail  (MetaOf syn)
               | MExpr  (MetaOf syn) (Expr syn)
                 deriving (Generic)

data Pat syn = PVar  (MetaOf syn) (IdentOf syn)
             | PWild (MetaOf syn)
             | PCon  (MetaOf syn) (IdentOf syn) [Pat syn]
               deriving (Generic)

data Expr syn = EVar (MetaOf syn) (IdentOf syn)
              | ECon (MetaOf syn) (IdentOf syn)
              | EApp (MetaOf syn) (Expr syn) [Expr syn]
              | EAbs (MetaOf syn) (Match syn)
              | ELit (MetaOf syn) (Literal syn)
              | ELet (MetaOf syn) [LetDecl syn] (Expr syn)
              | ECase (MetaOf syn) (Match syn)
                deriving (Generic)

data LetDecl syn = LDBind (MetaOf syn) (Bind syn)
                 | LDSig  (MetaOf syn) (Sig syn)
                   -- XXX add open declarations
                   deriving (Generic)

data Literal syn = LInt (MetaOf syn) Integer Int -- ^ value and base
                   deriving (Generic)

data Data syn = Data { dMeta    :: MetaOf syn
                     , dName    :: IdentOf syn
                     , dParams  :: [IdentOf syn]
                     , dConstrs :: [Constr syn]
                     } deriving (Generic)

data Constr syn = Constr { cMeta   :: MetaOf syn
                         , cName   :: IdentOf syn
                         , cParams :: [TypeOf syn]
                         } deriving (Generic)


data Schema syn = Schema (MetaOf syn) [IdentOf syn] (TypeOf syn)
                  deriving (Generic)

data Type syn = TCon (MetaOf syn) (IdentOf syn)
              | TVar (MetaOf syn) (IdentOf syn)
              | TApp (MetaOf syn) (Type syn) [Type syn]
              | TFun (MetaOf syn) (Type syn) (Type syn)
                deriving (Generic)


-- Helpers ---------------------------------------------------------------------

class HasSig f where
  isSig :: f syn -> Bool

instance HasSig Decl where
  isSig DSig{} = True
  isSig _      = False

instance HasSig LetDecl where
  isSig LDSig{} = True
  isSig _       = False


-- Instances -------------------------------------------------------------------

deriving instance Cxt Show syn => Show (Module    syn)
deriving instance Cxt Show syn => Show (Require   syn)
deriving instance Cxt Show syn => Show (ModStruct syn)
deriving instance Cxt Show syn => Show (ModSpec   syn)
deriving instance Cxt Show syn => Show (ModExpr   syn)
deriving instance Cxt Show syn => Show (ModType   syn)
deriving instance Cxt Show syn => Show (Decl      syn)
deriving instance Cxt Show syn => Show (Bind      syn)
deriving instance Cxt Show syn => Show (Sig       syn)
deriving instance Cxt Show syn => Show (Match     syn)
deriving instance Cxt Show syn => Show (Pat       syn)
deriving instance Cxt Show syn => Show (Expr      syn)
deriving instance Cxt Show syn => Show (LetDecl   syn)
deriving instance Cxt Show syn => Show (Literal   syn)
deriving instance Cxt Show syn => Show (Data      syn)
deriving instance Cxt Show syn => Show (Constr    syn)

-- front-end specific types and schemas
deriving instance Cxt Show syn => Show (Schema  syn)
deriving instance Cxt Show syn => Show (Type    syn)


-- Locations -------------------------------------------------------------------

instance HasRange (Module Parsed) where
  range = modMeta

instance HasRange (ModType Parsed) where
  range (MTVar     l _)     = l
  range (MTSig     l _)     = l
  range (MTFunctor l _ _ _) = l

instance HasRange (Sig Parsed) where
  range Sig { .. } = sigMeta

instance HasRange (Bind Parsed) where
  range Bind { .. } = bMeta

instance HasRange (Data Parsed) where
  range Data { .. } = dMeta

instance HasRange (Constr Parsed) where
  range Constr { .. } = cMeta

instance HasRange (ModSpec Parsed) where
  range (MSSig  l _)   = l
  range (MSKind l _)   = l
  range (MSData l _)   = l
  range (MSMod  l _ _) = l

instance HasRange (ModExpr Parsed) where
  range (MEName       l _)     = l
  range (MEApp        l _ _)   = l
  range (MEStruct     l _)     = l
  range (MEFunctor    l _ _ _) = l
  range (MEConstraint l _ _)   = l

instance HasRange (Match Parsed) where
  range (MPat   l _ _) = l
  range (MSplit l _ _) = l
  range (MFail  l)     = l
  range (MExpr  l _)   = l

instance HasRange (Schema Parsed) where
  range (Schema l _ _) = l

instance HasRange (Type Parsed) where
  range (TCon l _)   = l
  range (TVar l _)   = l
  range (TApp l _ _) = l
  range (TFun l _ _) = l

instance HasRange (Expr Parsed) where
  range (EVar l _)   = l
  range (ECon l _)   = l
  range (EApp l _ _) = l
  range (EAbs l _)   = l
  range (ELit l _)   = l
  range (ELet l _ _) = l
  range (ECase l _)  = l

instance HasRange (Pat Parsed) where
  range (PVar  l _)   = l
  range (PWild l)     = l
  range (PCon  l _ _) = l

instance HasRange (ModStruct Parsed) where
  range (ModStruct l _) = l

instance HasRange (Literal Parsed) where
  range (LInt l _ _) = l

instance HasRange PName where
  range (PUnqual l _) = l
  range (PQual l _ _) = l


-- Pretty-printing -------------------------------------------------------------

instance PP PName where
  ppr (PUnqual _ n)  = pp n
  ppr (PQual _ ns n) = vcat (intersperse (char '.') (map pp ns)) <> char '.' <> pp n
