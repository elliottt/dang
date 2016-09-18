{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}

module Dang.TypeCheck.AST where

import Dang.ModuleSystem.Name
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
