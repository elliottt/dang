{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}

module TypeChecker.Types where

import Pretty
import QualName
import Traversal (Data,Typeable)
import TypeChecker.Vars
import Variables

import Control.Applicative ((<$>),(<*>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Serialize
    (Get,Putter,getWord8,putWord8,getListOf,putListOf,getSetOf,putSetOf)
import Language.Haskell.TH.Syntax (Lift(..))
import qualified Data.Set as Set


-- Types -----------------------------------------------------------------------

data Type
  = TApp Type Type
  | TInfix QualName Type Type
  | TCon QualName
  | TVar (TVar Kind)
    deriving (Eq,Show,Ord,Data,Typeable)

instance Lift Type where
  lift ty = case ty of
    TApp f x      -> [| TApp   $(lift f)  $(lift x)           |]
    TInfix qn l r -> [| TInfix $(lift qn) $(lift l) $(lift r) |]
    TCon qn       -> [| TCon   $(lift qn)                     |]
    TVar tv       -> [| TVar   $(lift tv)                     |]

putType :: Putter Type
putType ty = case ty of
  TApp l r     -> putWord8 0 >> putType l     >> putType r
  TInfix n l r -> putWord8 1 >> putQualName n >> putType l >> putType r
  TCon n       -> putWord8 2 >> putQualName n
  TVar p       -> putWord8 3 >> putTVar putKind p

getType :: Get Type
getType  = getWord8 >>= \ tag ->
  case tag of
    0 -> TApp   <$> getType     <*> getType
    1 -> TInfix <$> getQualName <*> getType <*> getType
    2 -> TCon   <$> getQualName
    3 -> TVar   <$> getTVar getKind
    _ -> fail ("Invalid Type tag: " ++ show tag)

isTVar :: Type -> Bool
isTVar TVar{} = True
isTVar _      = False

instance Pretty Type where
  pp _ (TCon n)       = ppr n
  pp _ (TVar m)       = ppr m
  pp p (TApp a b)     = optParens (p > 1) (ppr a <+> pp 2 b)
  pp p (TInfix c a b) = optParens (p > 0) (pp 1 a <+> ppr c <+> pp 0 b)

instance FreeVars Type where
  freeVars (TCon qn)       = Set.singleton qn
  freeVars (TVar p)        = freeVars p
  freeVars (TApp a b)      = freeVars a `Set.union` freeVars b
  freeVars (TInfix qn a b) = Set.singleton qn `Set.union` freeVars (a,b)

-- | Map a function over the type variables in a type
mapTVar :: (TVar Kind-> TVar Kind) -> Type -> Type
mapTVar f = loop
  where
  loop (TApp a b)      = TApp (loop a) (loop b)
  loop (TInfix qn a b) = TInfix qn (loop a) (loop b)
  loop (TVar p)        = TVar (f p)
  loop ty              = ty

uvar :: TParam Kind -> Type
uvar  = TVar . UVar

gvar :: TParam Kind -> Type
gvar  = TVar . GVar

-- | Type-application introduction.
tapp :: Type -> Type -> Type
tapp  = TApp

-- | Arrow introduction.
tarrow :: Type -> Type -> Type
tarrow  = TInfix arrowConstr
infixr 9 `tarrow`

arrowConstr :: QualName
arrowConstr  = primName ["Prelude"] "->"

destInfix :: Type -> Maybe (QualName,Type,Type)
destInfix (TInfix qn l r) = return (qn,l,r)
destInfix _               = Nothing

destArrow :: Type -> Maybe (Type,Type)
destArrow ty = do
  (qn,l,r) <- destInfix ty
  guard (qn == arrowConstr)
  return (l,r)

destArgs :: Type -> [Type]
destArgs ty = fromMaybe [ty] $ do
  (l,r) <- destArrow ty
  return (l:destArgs r)

destTApp :: Type -> Maybe (Type,Type)
destTApp (TApp l r) = Just (l,r)
destTApp _          = Nothing

destTCon :: Type -> [Type]
destTCon ty = fromMaybe [ty] $ do
  (l,r) <- destTApp ty
  return (l:destTCon r)

destUVar :: Type -> Maybe (TParam Kind)
destUVar (TVar (UVar p)) = return p
destUVar _               = Nothing

-- | Count the number of arguments to a function.
typeArity :: Type -> Int
typeArity ty = maybe 0 rec (destArrow ty)
  where
  rec (_,r) = 1 + typeArity r


-- Constraints -----------------------------------------------------------------

type Constraint = Type
type Context    = Set.Set Constraint

getConstraint :: Get Constraint
getConstraint  = getType

putConstraint :: Putter Constraint
putConstraint  = putType

getContext :: Get Context
getContext  = getSetOf getConstraint

putContext :: Putter Context
putContext  = putSetOf putConstraint

emptyCxt :: Context
emptyCxt  = Set.empty

ppContext :: Context -> Doc
ppContext cxt
  | Set.null cxt = empty
  | otherwise    = parens (cat (map ppr (Set.toList cxt)))

-- | Type constructor for equality constraints.
eqConstr :: QualName
eqConstr  = primName ["Prelude"] "~"

-- | An equality constraint.
(~~) :: Type -> Type -> Constraint
(~~)  = TInfix eqConstr

destEq :: Constraint -> Maybe (Type,Type)
destEq c = case c of
  TInfix qn l r | qn == eqConstr -> Just (l,r)
  _                              -> Nothing


-- Kinds -----------------------------------------------------------------------

type Kind = Type

putKind :: Putter Kind
putKind  = putType

getKind :: Get Kind
getKind  = getType

-- | The kind of types.
kstar :: Kind
kstar  = TCon (primName [] "*")

-- | The kind of constraints/contexts
kcxt :: Kind
kcxt  = TCon (primName [] "Cxt")

-- | The kind of type constructors.
karrow :: Kind -> Kind -> Kind
karrow  = TInfix (primName [] "->")
infixr 9 `karrow`


-- Sorts -----------------------------------------------------------------------

type Sort = Type

setSort :: Sort
setSort = TCon (primName ["Prelude"] "Set")


-- Type Schemes ----------------------------------------------------------------

type Scheme = Forall (Qual Type)

-- | Produce a type scheme that quantifies no variables.
toScheme :: Type -> Scheme
toScheme  = toForall . toQual

putScheme :: Putter Scheme
putScheme  = putForall (putQual putType)

getScheme :: Get Scheme
getScheme  = getForall (getQual getType)


-- | Things with quantified variables.
data Forall a = Forall
  { forallParams :: [TParam Kind]
  , forallData   :: a
  } deriving (Show,Eq,Ord,Data,Typeable,Functor)

toForall :: a -> Forall a
toForall  = Forall []

putForall :: Putter a -> Putter (Forall a)
putForall p (Forall ps a) = putListOf (putTParam putKind) ps >> p a

getForall :: Get a -> Get (Forall a)
getForall a = Forall <$> getListOf (getTParam getKind) <*> a

instance Lift a => Lift (Forall a) where
  lift qa = [| Forall
    { forallParams = $(lift (forallParams qa))
    , forallData   = $(lift (forallData qa))
    } |]

instance Pretty a => Pretty (Forall a) where
  pp _ (Forall ps a) = vars <+> ppr a
    where
    vars | null ps   = empty
         | otherwise = text "forall" <+> ppList 0 ps <> char '.'

instance FreeVars a => FreeVars (Forall a) where
  freeVars = freeVars  . forallData


data Qual a = Qual
  { qualCxt  :: Context
  , qualData :: a
  } deriving (Show,Eq,Ord,Data,Typeable,Functor)

toQual :: a -> Qual a
toQual a = Qual emptyCxt a

putQual :: Putter a -> Putter (Qual a)
putQual p (Qual cxt a) = putContext cxt >> p a

getQual :: Get a -> Get (Qual a)
getQual m = Qual <$> getContext <*> m

instance Lift a => Lift (Qual a) where
  lift q = [| Qual
    { qualCxt  = Set.fromList $(lift (Set.toList (qualCxt q)))
    , qualData = $(lift (qualData q))
    } |]

instance Pretty a => Pretty (Qual a) where
  pp p (Qual cxt a) = optParens (p > 0) (cxtP <+> ppr a)
    where
    cxtP | Set.null cxt = empty
         | otherwise    = ppContext cxt <+> text "=>"

instance FreeVars a => FreeVars (Qual a) where
  freeVars (Qual cxt a) = freeVars cxt `Set.union` freeVars a
