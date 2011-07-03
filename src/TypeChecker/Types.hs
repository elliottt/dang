{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeChecker.Types where

import Pretty
import QualName

import Data.Int (Int64)

type Index = Int

data Type
  = TApp Type Type
  | TInfix QualName Type Type
  | TCon QualName
  | TVar Index TParam
  | TGen Index TParam
  | TNat Int64
    deriving (Eq,Show,Ord)

isTVar :: Type -> Bool
isTVar TVar{} = True
isTVar _      = False

instance Pretty Type where
  pp _ (TCon n)       = ppr n
  pp _ (TVar _ m)     = ppr m
  pp _ (TGen _ m)     = ppr m
  pp _ (TNat i)       = integer (fromIntegral i)
  pp p (TApp a b)     = optParens (p > 1) (ppr a <+> pp 2 b)
  pp p (TInfix c a b) = optParens (p > 0) (pp 2 a <+> ppr c <+> pp 2 b)

data TParam = TParam
  { paramName :: String
  , paramKind :: Kind
  } deriving (Eq,Show,Ord)

instance Pretty TParam where
  pp _ p = text (paramName p)

-- | Type-application introduction.
tapp :: Type -> Type -> Type
tapp  = TApp

-- | Arrow introduction.
tarrow :: Type -> Type -> Type
tarrow  = TInfix (primName "->")
infixr 9 `tarrow`

type Kind = Type

-- | The kind of types.
kstar :: Kind
kstar  = TCon (primName "*")

-- | The kind of type-naturals.
knat :: Kind
knat  = TCon (primName "#")

-- | The kind of type constructors.
karrow :: Kind -> Kind -> Kind
karrow  = TInfix (primName "->")
infixr 9 `karrow`

type Sort = Type

setSort :: Sort
setSort = TCon (primName "Set")


-- | Things with quantified variables.
data Forall a = Forall [TParam] a
    deriving (Show,Eq,Ord)

forallParams :: Forall a -> [TParam]
forallParams (Forall ps _) = ps

instance Pretty a => Pretty (Forall a) where
  pp _ (Forall ps a) = text "forall" <+> ppList 0 ps <> char '.' <+> pp 0 a
