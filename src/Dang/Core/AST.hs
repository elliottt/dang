{-# LANGUAGE DeriveGeneric #-}

module Dang.Core.AST where

import           Dang.Utils.PP

import           Data.Function (on)
import           GHC.Generics (Generic)


data Schema a = Schema [TParam] a
                deriving (Show,Generic)

type Kind = Type

data Type = TApp Type Type
          | TCon TyCon
          | TFree TParam
          | TGen TParam
            deriving (Eq,Show,Generic)

data TyCon = TyCon { tcName  :: String
                   , tcInfix :: Bool
                   , tcKind  :: Kind
                   } deriving (Show,Generic)

data TParam = TParam { tpName  :: String
                     , tpIndex :: !Int
                     , tpKind  :: Kind
                     } deriving (Show,Generic)

data Expr = EApp Expr [Expr]
          | EAbs 


-- Type Constants --------------------------------------------------------------

tcArrow :: TyCon
tcArrow  = TyCon { tcName = "->", tcInfix = True, tcKind = kSet }

tArrow :: Type -> Type -> Type
tArrow l r = TCon tcArrow `TApp` l `TApp` r


-- Kind Constants --------------------------------------------------------------

kSet :: Kind
kSet  = TCon TyCon { tcName = "Set", tcInfix = False, tcKind = kSet }

kArrow :: Kind -> Kind -> Kind
kArrow l r = TCon tcArrow `TApp` l `TApp` r


-- Utilities -------------------------------------------------------------------

instance Eq TyCon where
  (==) = (==) `on` tcName

instance Eq TParam where
  (==) = (==) `on` tpIndex
  (/=) = (/=) `on` tpIndex

instance Ord TParam where
  compare = compare `on` tpIndex

mapTGen :: (TParam -> TParam) -> Type -> Type
mapTGen f = go
  where
  go (TApp l r) = TApp (go l) (go r)
  go (TGen p)   = TGen (f p)
  go ty@TFree{} = ty
  go ty@TCon{}  = ty
