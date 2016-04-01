{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Dang.Syntax.AST where


import Dang.Syntax.Location
import Dang.Utils.PP

import           Control.Lens.Plated (Plated(..),gplate)
import           Data.List (intersperse)
import qualified Data.Text.Lazy as L
import           GHC.Generics (Generic)


-- | Parsed names, either qualified or unqualified.
data PName = PUnqual !L.Text
           | PQual   ![L.Text] !L.Text
             deriving (Eq,Show,Ord,Generic)

-- | A parsed top-level module.
type PModule = Module PName

data Module name = Module { modName  :: SrcLoc name
                          -- , modImports :: ?
                          , modDecls :: [Decl name]
                          } deriving (Show)

newtype ModStruct name = ModStruct { msElems :: [Decl name]
                                   } deriving (Eq,Show,Functor,Generic)

data Decl name = DBind    (Bind name)
               | DSig     (Sig name)
               | DData    (Data name)
               | DModBind (ModBind name)
               | DModType (ModType name)
                 -- XXX add open declarations
               | DLoc     (SrcLoc (Decl name))
                 deriving (Eq,Show,Functor,Generic)

data Bind name = Bind { bName   :: SrcLoc name
                      , bSchema :: Maybe (Schema name)
                      , bParams :: [Pat name]
                      , bBody   :: Expr name
                      } deriving (Eq,Show,Functor,Generic)

data Sig name = Sig { sigNames  :: [SrcLoc name]
                    , sigSchema :: SrcLoc (Schema name)
                    } deriving (Eq,Show,Functor,Generic)

data ModBind name = ModBind { mbName :: SrcLoc name
                            , mbExpr :: ModExpr name
                            } deriving (Eq,Show,Functor,Generic)

data ModType name = MTVar name
                  | MTSig [ModSpec name]
                  | MTFunctor (SrcLoc name) (ModType name) (ModType name)
                    -- XXX add with-constraints
                  | MTLoc (SrcLoc (ModType name))
                    deriving (Eq,Show,Functor,Generic)

data ModSpec name = MSSig (Sig name)
                  | MSData (Data name)
                  | MSMod (SrcLoc name) (ModType name)
                  | MSLoc (SrcLoc (ModSpec name))
                    deriving (Eq,Show,Functor,Generic)

data ModExpr name = MEName name
                  | MEApp (ModExpr name) (ModExpr name)
                  | MEStruct (ModStruct name)
                  | MEFunctor (SrcLoc name) (ModType name) (ModExpr name)
                  | MEConstraint (ModExpr name) (ModType name)
                  | MELoc (SrcLoc (ModExpr name))
                    deriving (Eq,Show,Functor,Generic)

data Match name = MPat (Pat name) (Match name)
                | MSplit (Match name) (Match name)
                | MFail
                | MExpr (Expr name)
                | MLoc (SrcLoc (Match name))
                  deriving (Eq,Show,Functor,Generic)

data Pat name = PVar (SrcLoc name)
              | PWild
              | PCon (SrcLoc name) [Pat name]
              | PLoc (SrcLoc (Pat name))
                deriving (Eq,Show,Functor,Generic)

data Expr name = EVar name
               | ECon name
               | EApp (Expr name) [Expr name]
               | EAbs (Match name)
               | ELit Literal
               | ELet [LetDecl name] (Expr name)
               | ELoc (SrcLoc (Expr name))
                 deriving (Eq,Show,Functor,Generic)

data LetDecl name = LDBind (Bind name)
                  | LDSig (Sig name)
                    -- XXX add open declarations
                  | LDLoc (SrcLoc (LetDecl name))
                    deriving (Eq,Show,Functor,Generic)

data Schema name = Schema [SrcLoc name] (Type name)
                   deriving (Eq,Show,Functor,Generic)

data Type name = TCon name
               | TVar name
               | TApp (Type name) [Type name]
               | TFun (Type name) (Type name)
               | TLoc (SrcLoc (Type name))
                 deriving (Eq,Show,Functor,Generic)

data Literal = LInt Integer Int -- ^ value and base
               deriving (Eq,Show,Generic)

data Data name = Data { dName    :: SrcLoc name
                      , dParams  :: [SrcLoc name]
                      , dConstrs :: [SrcLoc (Constr name)]
                      } deriving (Eq,Show,Functor,Generic)

data Constr name = Constr { cName   :: SrcLoc name
                          , cParams :: [Type name]
                          } deriving (Eq,Show,Functor,Generic)


-- Locations -------------------------------------------------------------------

instance HasLoc (ModType name) where
  type LocSource (ModType name) = Source
  getLoc (MTLoc loc) = getLoc loc
  getLoc _           = mempty

instance HasLoc (ModExpr name) where
  type LocSource (ModExpr name) = Source
  getLoc (MELoc loc) = getLoc loc
  getLoc _           = mempty

instance HasLoc (Type name) where
  type LocSource (Type name) = Source
  getLoc (TLoc loc) = getLoc loc
  getLoc _          = mempty

instance HasLoc (Pat name) where
  type LocSource (Pat name) = Source
  getLoc (PLoc loc) = getLoc loc
  getLoc _          = mempty

instance HasLoc (Match name) where
  type LocSource (Match name) = Source
  getLoc (MLoc loc) = getLoc loc
  getLoc _          = mempty

instance HasLoc (Expr name) where
  type LocSource (Expr name) = Source
  getLoc (ELoc loc) = getLoc loc
  getLoc _          = mempty

instance UnLoc (Decl name) where
  unLoc (DLoc l) = thing l
  unLoc d        = d

instance UnLoc (ModType name) where
  unLoc (MTLoc l) = thing l
  unLoc mt        = mt

instance UnLoc (ModSpec name) where
  unLoc (MSLoc l) = thing l
  unLoc ms        = ms

instance UnLoc (ModExpr name) where
  unLoc (MELoc l) = thing l
  unLoc me        = me

instance UnLoc (Pat name) where
  unLoc (PLoc l) = thing l
  unLoc p        = p

instance UnLoc (Expr name) where
  unLoc (ELoc l) = thing l
  unLoc e        = e

instance UnLoc (Type name) where
  unLoc (TLoc l) = thing l
  unLoc t        = t


-- Traversal -------------------------------------------------------------------

instance Plated (Decl    name) where plate = gplate
instance Plated (ModType name) where plate = gplate
instance Plated (ModSpec name) where plate = gplate
instance Plated (ModExpr name) where plate = gplate
instance Plated (Match   name) where plate = gplate
instance Plated (Pat     name) where plate = gplate
instance Plated (Expr    name) where plate = gplate
instance Plated (Type    name) where plate = gplate


-- Pretty-printing -------------------------------------------------------------

instance PP PName where
  ppr (PUnqual n)  = pp n
  ppr (PQual ns n) = vcat (intersperse (char '.') (map pp ns)) <> char '.' <> pp n
