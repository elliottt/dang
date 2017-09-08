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
import qualified Data.Text.Lazy as L
import           GHC.Generics (Generic)


-- Syntax and Metadata ---------------------------------------------------------

-- | The syntax descriptor for parsed modules.
data Parsed

type instance IdentOf  Parsed = SrcLoc PName
type instance TypeOf   Parsed = Type Parsed
type instance SchemaOf Parsed = Schema Parsed
type instance MetaOf   Parsed = SrcRange


-- AST -------------------------------------------------------------------------

-- | Parsed names, either qualified or unqualified.
data PName = PUnqual !L.Text
           | PQual   ![L.Text] !L.Text
             deriving (Eq,Show,Ord,Generic)

pnameNamespace :: PName -> [L.Text]
pnameNamespace (PUnqual i)  = [i]
pnameNamespace (PQual ns i) = ns ++ [i]

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

instance HasLoc (Module Parsed) where
  type LocSource (Module Parsed) = Source
  getLoc = modMeta

instance HasLoc (ModType Parsed) where
  type LocSource (ModType Parsed) = Source
  getLoc (MTVar     l _)     = l
  getLoc (MTSig     l _)     = l
  getLoc (MTFunctor l _ _ _) = l

instance HasLoc (Sig Parsed) where
  type LocSource (Sig Parsed) = Source
  getLoc Sig { .. } = sigMeta

instance HasLoc (Bind Parsed) where
  type LocSource (Bind Parsed) = Source
  getLoc Bind { .. } = bMeta

instance HasLoc (Data Parsed) where
  type LocSource (Data Parsed) = Source
  getLoc Data { .. } = dMeta

instance HasLoc (Constr Parsed) where
  type LocSource (Constr Parsed) = Source
  getLoc Constr { .. } = cMeta

instance HasLoc (ModExpr Parsed) where
  type LocSource (ModExpr Parsed) = Source
  getLoc (MEName       l _)     = l
  getLoc (MEApp        l _ _)   = l
  getLoc (MEStruct     l _)     = l
  getLoc (MEFunctor    l _ _ _) = l
  getLoc (MEConstraint l _ _)   = l

instance HasLoc (Match Parsed) where
  type LocSource (Match Parsed) = Source
  getLoc (MPat   l _ _) = l
  getLoc (MSplit l _ _) = l
  getLoc (MFail  l)     = l
  getLoc (MExpr  l _)   = l

instance HasLoc (Schema Parsed) where
  type LocSource (Schema Parsed) = Source
  getLoc (Schema l _ _) = l

instance HasLoc (Type Parsed) where
  type LocSource (Type Parsed) = Source
  getLoc (TCon l _)   = l
  getLoc (TVar l _)   = l
  getLoc (TApp l _ _) = l
  getLoc (TFun l _ _) = l

instance HasLoc (Expr Parsed) where
  type LocSource (Expr Parsed) = Source
  getLoc (EVar l _)   = l
  getLoc (ECon l _)   = l
  getLoc (EApp l _ _) = l
  getLoc (EAbs l _)   = l
  getLoc (ELit l _)   = l
  getLoc (ELet l _ _) = l

instance HasLoc (Pat Parsed) where
  type LocSource (Pat Parsed) = Source
  getLoc (PVar  l _)   = l
  getLoc (PWild l)     = l
  getLoc (PCon  l _ _) = l

instance HasLoc (ModStruct Parsed) where
  type LocSource (ModStruct Parsed) = Source
  getLoc (ModStruct l _) = l

instance HasLoc (Literal Parsed) where
  type LocSource (Literal Parsed) = Source
  getLoc (LInt l _ _) = l


-- Pretty-printing -------------------------------------------------------------

instance PP PName where
  ppr (PUnqual n)  = pp n
  ppr (PQual ns n) = vcat (intersperse (char '.') (map pp ns)) <> char '.' <> pp n
