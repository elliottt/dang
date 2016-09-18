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
data Parsed ident

instance Syn (Parsed ident) where
  type IdentOf  (Parsed ident)   = ident
  type TypeOf   (Parsed ident)   = Type (Parsed ident)
  type SchemaOf (Parsed ident)   = Schema (Parsed ident)

  type MetaOf   (Parsed ident) Module    = SrcRange
  type MetaOf   (Parsed ident) ModStruct = SrcRange
  type MetaOf   (Parsed ident) Decl      = SrcRange
  type MetaOf   (Parsed ident) Bind      = SrcRange
  type MetaOf   (Parsed ident) Sig       = SrcRange
  type MetaOf   (Parsed ident) ModBind   = SrcRange
  type MetaOf   (Parsed ident) ModType   = SrcRange
  type MetaOf   (Parsed ident) ModSpec   = SrcRange
  type MetaOf   (Parsed ident) ModExpr   = SrcRange
  type MetaOf   (Parsed ident) Match     = SrcRange
  type MetaOf   (Parsed ident) Pat       = SrcRange
  type MetaOf   (Parsed ident) Expr      = SrcRange
  type MetaOf   (Parsed ident) LetDecl   = SrcRange
  type MetaOf   (Parsed ident) Schema    = SrcRange
  type MetaOf   (Parsed ident) Type      = SrcRange
  type MetaOf   (Parsed ident) Literal   = SrcRange
  type MetaOf   (Parsed ident) Data      = SrcRange
  type MetaOf   (Parsed ident) Constr    = SrcRange


-- AST -------------------------------------------------------------------------

-- | Parsed names, either qualified or unqualified.
data PName = PUnqual !L.Text
           | PQual   ![L.Text] !L.Text
             deriving (Eq,Show,Ord,Generic)

-- | A parsed top-level module.
type PModule = Module (Parsed (SrcLoc PName))

type P = Parsed (SrcLoc PName)

data Module syn = Module { modMeta  :: MetaOf syn Module
                         , modName  :: IdentOf syn
                         -- , modImports :: ?
                         , modDecls :: [Decl syn]
                         } deriving (Generic)

data ModStruct syn = ModStruct { msMeta  :: MetaOf syn ModStruct
                               , msElems :: [Decl syn]
                               } deriving (Generic)

data Decl syn = DBind    (MetaOf syn Decl) (Bind syn)
              | DSig     (MetaOf syn Decl) (Sig syn)
              | DData    (MetaOf syn Decl) (Data syn)
              | DModBind (MetaOf syn Decl) (ModBind syn)
              | DModType (MetaOf syn Decl) (ModType syn)
                deriving (Generic)

data Bind syn = Bind { bMeta   :: MetaOf syn Bind
                     , bName   :: IdentOf syn
                     , bParams :: [Pat syn]
                     , bBody   :: Expr syn
                     } deriving (Generic)

data Sig syn = Sig { sigMeta   :: MetaOf syn Sig
                   , sigNames  :: [IdentOf syn]
                   , sigSchema :: SchemaOf syn
                   } deriving (Generic)

data ModBind syn = ModBind { mbMeta :: MetaOf syn ModBind
                           , mbName :: IdentOf syn
                           , mbExpr :: ModExpr syn
                           } deriving (Generic)

data ModType syn = MTVar     (MetaOf syn ModType) (IdentOf syn)
                 | MTSig     (MetaOf syn ModType) [ModSpec syn]
                 | MTFunctor (MetaOf syn ModType) (IdentOf syn) (ModType syn) (ModType syn)
                   -- XXX add with-constraints
                   deriving (Generic)

data ModSpec syn = MSSig  (MetaOf syn ModSpec) (Sig syn)
                 | MSData (MetaOf syn ModSpec) (Data syn)
                 | MSMod  (MetaOf syn ModSpec) (IdentOf syn) (ModType syn)
                   deriving (Generic)

data ModExpr syn = MEName       (MetaOf syn ModExpr) (IdentOf syn)
                 | MEApp        (MetaOf syn ModExpr) (ModExpr syn) (ModExpr syn)
                 | MEStruct     (MetaOf syn ModExpr) (ModStruct syn)
                 | MEFunctor    (MetaOf syn ModExpr) (IdentOf syn) (ModType syn) (ModExpr syn)
                 | MEConstraint (MetaOf syn ModExpr) (ModExpr syn) (ModType syn)
                   deriving (Generic)

data Match syn = MPat   (MetaOf syn Match) (Pat syn) (Match syn)
               | MSplit (MetaOf syn Match) (Match syn) (Match syn)
               | MFail  (MetaOf syn Match)
               | MExpr  (MetaOf syn Match) (Expr syn)
                 deriving (Generic)

data Pat syn = PVar  (MetaOf syn Pat) (IdentOf syn)
             | PWild (MetaOf syn Pat)
             | PCon  (MetaOf syn Pat) (IdentOf syn) [Pat syn]
               deriving (Generic)

data Expr syn = EVar (MetaOf syn Expr) (IdentOf syn)
              | ECon (MetaOf syn Expr) (IdentOf syn)
              | EApp (MetaOf syn Expr) (Expr syn) [Expr syn]
              | EAbs (MetaOf syn Expr) (Match syn)
              | ELit (MetaOf syn Expr) (Literal syn)
              | ELet (MetaOf syn Expr) [LetDecl syn] (Expr syn)
                deriving (Generic)

data LetDecl syn = LDBind (MetaOf syn LetDecl) (Bind syn)
                 | LDSig  (MetaOf syn LetDecl) (Sig syn)
                   -- XXX add open declarations
                   deriving (Generic)

data Literal syn = LInt (MetaOf syn Literal) Integer Int -- ^ value and base
                   deriving (Generic)

data Data syn = Data { dMeta    :: MetaOf syn Data
                     , dName    :: IdentOf syn
                     , dParams  :: [IdentOf syn]
                     , dConstrs :: [Constr syn]
                     } deriving (Generic)

data Constr syn = Constr { cMeta   :: MetaOf syn Constr
                         , cName   :: IdentOf syn
                         , cParams :: [TypeOf syn]
                         } deriving (Generic)


data Schema syn = Schema (MetaOf syn Schema) [IdentOf syn] (TypeOf syn)
                  deriving (Generic)

data Type syn = TCon (MetaOf syn Type) (IdentOf syn)
              | TVar (MetaOf syn Type) (IdentOf syn)
              | TApp (MetaOf syn Type) (Type syn) [Type syn]
              | TFun (MetaOf syn Type) (Type syn) (Type syn)
                deriving (Generic)


-- Instances -------------------------------------------------------------------

type Cxt f syn = All f syn '[Bind,Expr,LetDecl,Sig,Match,Pat,Literal,Data,Constr
                            ,Decl,ModType,ModSpec,ModBind,ModExpr,ModStruct
                            ,Module]

deriving instance Cxt Show syn => Show (Module    syn)
deriving instance Cxt Show syn => Show (ModStruct syn)
deriving instance Cxt Show syn => Show (ModBind   syn)
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
deriving instance All Show syn '[Schema,Type] => Show (Schema  syn)
deriving instance All Show syn '[Type]        => Show (Type    syn)


-- Locations -------------------------------------------------------------------

instance HasLoc (Module (Parsed ident)) where
  type LocSource (Module (Parsed ident)) = Source
  getLoc = modMeta

instance HasLoc (ModType (Parsed ident)) where
  type LocSource (ModType (Parsed ident)) = Source
  getLoc (MTVar     l _)     = l
  getLoc (MTSig     l _)     = l
  getLoc (MTFunctor l _ _ _) = l

instance HasLoc (Sig (Parsed ident)) where
  type LocSource (Sig (Parsed ident)) = Source
  getLoc Sig { .. } = sigMeta

instance HasLoc (Bind (Parsed ident)) where
  type LocSource (Bind (Parsed ident)) = Source
  getLoc Bind { .. } = bMeta

instance HasLoc (Data (Parsed ident)) where
  type LocSource (Data (Parsed ident)) = Source
  getLoc Data { .. } = dMeta

instance HasLoc (Constr (Parsed ident)) where
  type LocSource (Constr (Parsed ident)) = Source
  getLoc Constr { .. } = cMeta

instance HasLoc (ModBind (Parsed ident)) where
  type LocSource (ModBind (Parsed ident)) = Source
  getLoc ModBind { .. } = mbMeta

instance HasLoc (ModExpr (Parsed ident)) where
  type LocSource (ModExpr (Parsed ident)) = Source
  getLoc (MEName       l _)     = l
  getLoc (MEApp        l _ _)   = l
  getLoc (MEStruct     l _)     = l
  getLoc (MEFunctor    l _ _ _) = l
  getLoc (MEConstraint l _ _)   = l

instance HasLoc (Match (Parsed ident)) where
  type LocSource (Match (Parsed ident)) = Source
  getLoc (MPat   l _ _) = l
  getLoc (MSplit l _ _) = l
  getLoc (MFail  l)     = l
  getLoc (MExpr  l _)   = l

instance HasLoc (Schema (Parsed ident)) where
  type LocSource (Schema (Parsed ident)) = Source
  getLoc (Schema l _ _) = l

instance HasLoc (Type (Parsed ident)) where
  type LocSource (Type (Parsed ident)) = Source
  getLoc (TCon l _)   = l
  getLoc (TVar l _)   = l
  getLoc (TApp l _ _) = l
  getLoc (TFun l _ _) = l

instance HasLoc (Expr (Parsed ident)) where
  type LocSource (Expr (Parsed ident)) = Source
  getLoc (EVar l _)   = l
  getLoc (ECon l _)   = l
  getLoc (EApp l _ _) = l
  getLoc (EAbs l _)   = l
  getLoc (ELit l _)   = l
  getLoc (ELet l _ _) = l

instance HasLoc (Pat (Parsed ident)) where
  type LocSource (Pat (Parsed ident)) = Source
  getLoc (PVar  l _)   = l
  getLoc (PWild l)     = l
  getLoc (PCon  l _ _) = l

instance HasLoc (ModStruct (Parsed ident)) where
  type LocSource (ModStruct (Parsed ident)) = Source
  getLoc (ModStruct l _) = l

instance HasLoc (Literal (Parsed ident)) where
  type LocSource (Literal (Parsed ident)) = Source
  getLoc (LInt l _ _) = l


-- Pretty-printing -------------------------------------------------------------

instance PP PName where
  ppr (PUnqual n)  = pp n
  ppr (PQual ns n) = vcat (intersperse (char '.') (map pp ns)) <> char '.' <> pp n
