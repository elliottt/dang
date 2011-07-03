module TypeChecker.Types where

import Pretty

import Data.Int (Int64)


data Type
  = TApp Type Type
  | TInfix String Type Type
  | TCon String
  | TVar Int TParam
  | TGen Int TParam
  | TNat Int64
    deriving (Eq,Show,Ord)

isTVar :: Type -> Bool
isTVar TVar{} = True
isTVar _      = False

instance Pretty Type where
  pp _ (TCon n)       = text n
  pp _ (TVar _ m)     = pp 0 m
  pp _ (TGen _ m)     = pp 0 m
  pp _ (TNat i)       = integer (fromIntegral i)
  pp p (TApp a b)     = optParens (p > 1) (pp 0 a <+> pp 2 b)
  pp p (TInfix c a b) = optParens (p > 0) (pp 2 a <+> text c <+> pp 2 b)

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
tarrow  = TInfix "->"

type Kind = Type

-- | The kind of types.
kstar :: Kind
kstar  = TCon "*"

-- | The kind of type-naturals.
knat :: Kind
knat  = TCon "#"

-- | The kind of type constructors.
karrow :: Kind -> Kind -> Kind
karrow  = TInfix "->"

type Sort = Type

setSort :: Sort
setSort = TCon "Set"


-- | Things with quantified variables.
data Forall a = Forall [TParam] a
    deriving (Show,Eq,Ord)

instance Pretty a => Pretty (Forall a) where
  pp _ (Forall ps a) = text "forall" <+> ppList 0 ps <> char '.' <+> pp 0 a
