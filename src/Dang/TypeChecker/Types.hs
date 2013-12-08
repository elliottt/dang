{-# LANGUAGE DeriveDataTypeable #-}

module Dang.TypeChecker.Types where

import Dang.ModuleSystem.QualName
import Dang.Traversal (Data,Typeable)
import Dang.Utils.Pretty

import Control.Monad (guard)
import Data.Maybe (fromMaybe)


-- Types -----------------------------------------------------------------------

data Type
  = TApp Type Type
  | TCon Name [Type]
  | TVar TParam
  | TGen TParam
    deriving (Eq,Show,Ord,Data,Typeable)

instance Pretty Type where
  ppr (TApp l r)     = optParens 10 (pp   l <+> ppPrec 10 r)
  ppr (TCon con tys) = optParens 10 (pp con <+> hsep (map (ppPrec 10) tys))
  ppr (TVar tp)      = pp tp
  ppr (TGen tp)      = pp tp

data TParam = TParam { tpName  :: Maybe String
                     , tpIndex :: Int
                     , tpKind  :: Kind
                     } deriving (Show,Eq,Ord,Data,Typeable)

instance Pretty TParam where
  ppr tp = maybe iname text (tpName tp)
    where
    iname = char 't' <> int (tpIndex tp)

elimTCon :: Type -> Maybe (Name,[Type])
elimTCon ty = case ty of
  TCon f xs -> Just (f,xs)
  _         -> Nothing

tArrowCon :: Name
tArrowCon  = mkQual (Type 0) ["Prelude"] "->"

-- | Arrow introduction.
tArrow :: Type -> Type -> Type
tArrow a b = TCon tArrowCon [a,b]
infixr 9 `tArrow`

-- | Arrow elimination.
elimArrow :: Type -> Maybe (Type,Type)
elimArrow ty = do
  (f,xs) <- elimTCon ty
  guard (f == tArrowCon)
  case xs of
    [l,r] -> return (l,r)
    _     -> Nothing

elimArrows :: Type -> [Type]
elimArrows ty = fromMaybe [ty] $ do
  (l,r) <- elimArrow ty
  return (l:elimArrows r)

-- | Count the number of arguments to a function.
typeArity :: Type -> Int
typeArity ty = length (elimArrows ty)


-- Kinds -----------------------------------------------------------------------

type Kind = Type

-- | The kind of types.
kStar :: Kind
kStar  = TCon (mkQual (Type 1) ["Prelude"] "*") []

kArrowCon :: Name
kArrowCon  = mkQual (Type 1) ["Prelude"] "->"

-- | The kind of type constructors.
kArrow :: Kind -> Kind -> Kind
kArrow a b = TCon kArrowCon [a,b]
infixr 9 `kArrow`


-- Sorts -----------------------------------------------------------------------

type Sort = Type

-- | The type of kinds.
sSet :: Sort
sSet = TCon (mkQual (Type 2) ["Prelude"] "Set") []


-- Type Schemes ----------------------------------------------------------------

-- | The kind of constraints/contexts
kProp :: Kind
kProp  = TCon (mkQual (Type 1) ["Prelude"] "Prop") []

-- | Type constraints
type Prop = Type

pEqCon :: Name
pEqCon  = mkQual (Type 0) ["Prelude"] "~"

-- | An equality constraint.
(~~) :: Type -> Type -> Prop
(~~) a b = TCon pEqCon [a,b]
infix 1 ~~


-- | Things with quantified variables.
data Schema = Forall
  { sParams :: [TParam]
  , sProps  :: [Prop]
  , sType   :: Type
  } deriving (Show,Eq,Ord,Data,Typeable)

toSchema :: Type -> Schema
toSchema  = Forall [] []

instance Pretty Schema where
  ppr s = optParens 10 (vars <+> context <+> pp (sType s))
    where
    vars | null (sParams s) = empty
         | otherwise        = text "forall" <+> fsep (map pp (sParams s))
                                             <> char '.'

    context | null (sProps s) = empty
            | otherwise       = fsep (commas (map pp (sProps s))) <+> text "=>"

    commas = list (char '(') comma (char ')')
