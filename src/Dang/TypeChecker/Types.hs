{-# LANGUAGE DeriveDataTypeable #-}

module Dang.TypeChecker.Types where

import Dang.ModuleSystem.QualName
import Dang.Utils.Pretty
import Dang.Variables

import Control.Monad (guard)
import Data.Data ( Data )
import Data.Maybe (fromMaybe)
import Data.Typeable ( Typeable )
import qualified Data.Set as Set


-- Types -----------------------------------------------------------------------

data Type
  = TApp Type Type
  | TCon Name
  | TVar TParam
  | TGen TParam
    deriving (Eq,Show,Ord,Data,Typeable)

instance Pretty Type where
  ppr (TApp l r) = optParens 10 (pp   l <+> ppPrec 10 r)
  ppr (TCon con) = pp con
  ppr (TVar tp)  = pp tp
  ppr (TGen tp)  = pp tp

instance FreeVars Type where
  freeVars (TApp l r) = freeVars (l,r)
  freeVars (TCon n)   = Set.singleton n
  freeVars (TVar _)   = Set.empty
  freeVars (TGen _)   = Set.empty

data TParam = TParam { tpName  :: Maybe String
                     , tpIndex :: Int
                     , tpKind  :: Kind
                     } deriving (Show,Eq,Ord,Data,Typeable)

instance Pretty TParam where
  ppr tp = maybe iname text (tpName tp)
    where
    iname = char 't' <> int (tpIndex tp)

elimTCon :: Type -> Maybe Name
elimTCon ty = case ty of
  TCon n -> Just n
  _      -> Nothing

elimTApp :: Type -> (Type,[Type])
elimTApp  = go []
  where
  go acc (TApp f x) = go (x:acc) f
  go acc ty         = (ty,acc)

tapp :: Type -> [Type] -> Type
tapp f xs = foldl TApp f xs

tArrowCon :: Name
tArrowCon  = mkQual (Type 0) ["Prelude"] "->"

-- | Arrow introduction.
tArrow :: Type -> Type -> Type
tArrow a b = tapp (TCon tArrowCon) [a,b]
infixr 9 `tArrow`

-- | Arrow elimination.
elimArrow :: Type -> Maybe (Type,Type)
elimArrow ty = do
  let (f,xs) = elimTApp ty
  con <- elimTCon f
  guard (con == tArrowCon)
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
kSet :: Kind
kSet  = TCon (mkQual (Type 1) ["Prelude"] "Set")

kArrowCon :: Name
kArrowCon  = mkQual (Type 1) ["Prelude"] "->"

-- | The kind of type constructors.
kArrow :: Kind -> Kind -> Kind
kArrow a b = tapp (TCon kArrowCon) [a,b]
infixr 9 `kArrow`

-- | The kind of constraints/contexts
kProp :: Kind
kProp  = TCon (mkQual (Type 1) ["Prelude"] "Prop")

-- | The kind of row-types
kRow :: Kind
kRow  = TCon (mkQual (Type 1) ["Prelude"] "Row")


-- Sorts -----------------------------------------------------------------------

type Sort = Type

-- | The type of kinds.
sSet :: Sort
sSet = TCon (mkQual (Type 2) ["Prelude"] "Set")


-- Type Schemas ----------------------------------------------------------------

-- | Type constraints
type Prop = Type

pEqCon :: Name
pEqCon  = mkQual (Type 0) ["Prelude"] "~"

-- | An equality constraint.
(~~) :: Type -> Type -> Prop
(~~) a b = tapp (TCon pEqCon) [a,b]
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
            | otherwise       = fsep (tuple (map pp (sProps s))) <+> text "=>"

    tuple = list (char '(') comma (char ')')
