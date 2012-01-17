{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module TypeChecker.Types where

import Pretty
import QualName
import Variables

import Control.Applicative ((<$>),(<*>))
import Control.Monad (guard)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Serialize
    (get,put,Get,Putter,getWord8,putWord8,getWord32be,putWord32be,getListOf
    ,putListOf)
import Language.Haskell.TH.Syntax (Lift(..),liftString)
import qualified Data.Set as Set


-- Type Indices ----------------------------------------------------------------

type Index = Int

putIndex :: Putter Index
putIndex  = putWord32be . toEnum

getIndex :: Get Index
getIndex  = fromEnum <$> getWord32be


-- Types -----------------------------------------------------------------------

data Type
  = TApp Type Type
  | TInfix QualName Type Type
  | TCon QualName
  | TVar TVar
    deriving (Eq,Show,Ord)

instance Lift Type where
  lift ty = case ty of
    TApp f x      -> [| TApp   $(lift f)  $(lift x)           |]
    TInfix qn l r -> [| TInfix $(lift qn) $(lift l) $(lift r) |]
    TCon qn       -> [| TCon   $(lift qn)                     |]
    TVar tv       -> [| TVar   $(lift tv)                     |]

putType :: Putter Type
putType (TApp l r)     = putWord8 0 >> putType l     >> putType r
putType (TInfix n l r) = putWord8 1 >> putQualName n >> putType l >> putType r
putType (TCon n)       = putWord8 2 >> putQualName n
putType (TVar p)       = putWord8 3 >> putTVar p

getType :: Get Type
getType  = getWord8 >>= \ tag ->
  case tag of
    0 -> TApp   <$> getType     <*> getType
    1 -> TInfix <$> getQualName <*> getType <*> getType
    2 -> TCon   <$> getQualName
    3 -> TVar   <$> getTVar
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
mapTVar :: (TVar -> TVar) -> Type -> Type
mapTVar f = loop
  where
  loop (TApp a b)      = TApp (loop a) (loop b)
  loop (TInfix qn a b) = TInfix qn (loop a) (loop b)
  loop (TVar p)        = TVar (f p)
  loop ty              = ty

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

destUVar :: Type -> Maybe TParam
destUVar (TVar (UVar p)) = return p
destUVar _               = Nothing

-- | Count the number of arguments to a function.
typeArity :: Type -> Int
typeArity ty = maybe 0 rec (destArrow ty)
  where
  rec (_,r) = 1 + typeArity r


-- Type Variables --------------------------------------------------------------

data TVar
  = GVar TParam
  | UVar TParam
    deriving (Show,Eq,Ord)

instance Lift TVar where
  lift tv = case tv of
    UVar p -> [| UVar $(lift p) |]
    GVar p -> [| GVar $(lift p) |]

putTVar :: Putter TVar
putTVar (GVar p) = putWord8 0 >> putTParam p
putTVar (UVar p) = putWord8 1 >> putTParam p

getTVar :: Get TVar
getTVar  = getWord8 >>= \ tag -> case tag of
  0 -> GVar <$> getTParam
  1 -> UVar <$> getTParam
  _ -> fail ("Invalid TVar tag: " ++ show tag)

instance Pretty TVar where
  pp p (GVar v) = pp p v
  pp p (UVar v) = pp p v

instance FreeVars TVar where
  freeVars (UVar v) = Set.singleton (simpleName (paramName v))
  freeVars GVar{}   = Set.empty

uvar :: TParam -> Type
uvar  = TVar . UVar

gvar :: TParam -> Type
gvar  = TVar . GVar


-- Type Parameters -------------------------------------------------------------

data TParam = TParam
  { paramIndex      :: Index
  , paramFromSource :: Bool
  , paramName       :: String
  , paramKind       :: Kind
  } deriving (Show)

instance Lift TParam where
  lift tp = [| TParam
    { paramIndex      = $(lift       (paramIndex tp))
    , paramFromSource = $(lift       (paramFromSource tp))
    , paramName       = $(liftString (paramName tp))
    , paramKind       = $(lift       (paramKind tp))
    } |]

instance Eq TParam where
  (==) = (==) `on` paramIndex
  (/=) = (/=) `on` paramIndex

instance Ord TParam where
  compare = comparing paramIndex

instance FreeVars TParam where
  freeVars = Set.singleton . simpleName . paramName

instance Pretty TParam where
  pp _ p = text (paramName p)

setTParamIndex :: Index -> TParam -> TParam
setTParamIndex ix p = p { paramIndex = ix }

putTParam :: Putter TParam
putTParam p = putIndex (paramIndex p)
           >> put (paramFromSource p)
           >> put (paramName p)
           >> putKind (paramKind p)

getTParam :: Get TParam
getTParam  = TParam <$> getIndex <*> get <*> get <*> getKind


-- Kinds -----------------------------------------------------------------------

type Kind = Type

putKind :: Putter Kind
putKind  = putType

getKind :: Get Kind
getKind  = getType

-- | The kind of types.
kstar :: Kind
kstar  = TCon (primName [] "*")

-- | The kind of type constructors.
karrow :: Kind -> Kind -> Kind
karrow  = TInfix (primName [] "->")
infixr 9 `karrow`


-- Sorts -----------------------------------------------------------------------

type Sort = Type

setSort :: Sort
setSort = TCon (primName ["Prelude"] "Set")


-- Type Schemes ----------------------------------------------------------------

type Scheme = Forall Type

-- | Produce a type scheme that quantifies no variables.
toScheme :: Type -> Scheme
toScheme  = Forall []

-- | Things with quantified variables.
data Forall a = Forall
  { forallParams :: [TParam]
  , forallData   :: a
  } deriving (Show,Eq,Ord)

instance Lift a => Lift (Forall a) where
  lift qa = [| Forall
    { forallParams = $(lift (forallParams qa))
    , forallData   = $(lift (forallData qa))
    } |]

putForall :: Putter a -> Putter (Forall a)
putForall p (Forall ps a) = putListOf putTParam ps >> p a

getForall :: Get a -> Get (Forall a)
getForall a = Forall <$> getListOf getTParam <*> a

instance Pretty a => Pretty (Forall a) where
  pp _ (Forall ps a) = vars <+> pp 0 a
    where
    vars | null ps   = empty
         | otherwise = text "forall" <+> ppList 0 ps <> char '.'

instance FreeVars a => FreeVars (Forall a) where
  freeVars = freeVars  . forallData
