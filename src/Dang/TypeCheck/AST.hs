{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

-- this is for the pass through of the MetaOf instance for Checked
{-# LANGUAGE UndecidableInstances #-}

module Dang.TypeCheck.AST where

import Dang.AST
import Dang.ModuleSystem.Name
import Dang.Syntax.AST hiding (Type(..),Schema,TVar)
import Dang.Syntax.Location
import Dang.Utils.PP

import GHC.Generics (Generic)


newtype TVar = TVar { tvName :: Name
                    } deriving (Eq,Ord,Show,Generic)

data Schema = Forall [TVar] Type
              deriving (Eq,Ord,Show,Generic)

data Type = TFree !TVar
          | TGen !TVar
          | TCon !Name
          | TApp !Type !Type
          | TFun !Type !Type
            deriving (Eq,Ord,Show,Generic)

data BindMeta = BindMeta SrcRange Schema
                deriving (Show)

instance HasLoc BindMeta where
  type LocSource BindMeta = Source
  getLoc (BindMeta l _) = l


-- AST -------------------------------------------------------------------------

data Checked

instance Syn Checked where
  type IdentOf  Checked           = Name
  type TypeOf   Checked           = Type
  type SchemaOf Checked           = Schema

  type MetaOf   Checked Module    = SrcRange
  type MetaOf   Checked ModStruct = SrcRange
  type MetaOf   Checked Decl      = SrcRange
  type MetaOf   Checked Bind      = BindMeta
  type MetaOf   Checked Sig       = SrcRange
  type MetaOf   Checked ModType   = SrcRange
  type MetaOf   Checked ModSpec   = SrcRange
  type MetaOf   Checked ModExpr   = SrcRange
  type MetaOf   Checked Match     = SrcRange
  type MetaOf   Checked Pat       = SrcRange
  type MetaOf   Checked Expr      = SrcRange
  type MetaOf   Checked LetDecl   = SrcRange
  type MetaOf   Checked Schema    = SrcRange
  type MetaOf   Checked Type      = SrcRange
  type MetaOf   Checked Literal   = SrcRange
  type MetaOf   Checked Data      = SrcRange
  type MetaOf   Checked Constr    = SrcRange


-- Pretty-printing -------------------------------------------------------------

instance PP TVar where
  ppr (TVar n) = pp n

instance PP Type where
  ppr (TFree v) = char '?' <> ppr v
  ppr (TGen v)  = ppr v
  ppr (TCon n)  = ppr n
  ppr (TApp f x) = optParens 10 (hang (pp f) 2 (ppPrec 10 x))
  ppr (TFun a b) = optParens 10 (hang (ppPrec 10 a) 2
                                      (text "->" <+> pp b))
