{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeChecker.Types where

import Pretty
import QualName

import Control.Applicative ((<$>),(<*>))
import Control.Monad (guard)
import Data.Serialize
    (get,put,Get,Putter,getWord8,putWord8,getWord32be,putWord32be,getListOf
    ,putListOf)

type Index = Int

putIndex :: Putter Index
putIndex  = putWord32be . toEnum

getIndex :: Get Index
getIndex  = fromEnum <$> getWord32be

data Type
  = TApp Type Type
  | TInfix QualName Type Type
  | TCon QualName
  | TVar Index TParam
  | TGen Index TParam
    deriving (Eq,Show,Ord)

putType :: Putter Type
putType (TApp l r)     = putWord8 0 >> putType l     >> putType r
putType (TInfix n l r) = putWord8 1 >> putQualName n >> putType l >> putType r
putType (TCon n)       = putWord8 2 >> putQualName n
putType (TVar i p)     = putWord8 3 >> putIndex i    >> putTParam p
putType (TGen i p)     = putWord8 4 >> putIndex i    >> putTParam p

getType :: Get Type
getType  = getWord8 >>= \ tag ->
  case tag of
    0 -> TApp   <$> getType     <*> getType
    1 -> TInfix <$> getQualName <*> getType <*> getType
    2 -> TCon   <$> getQualName
    3 -> TVar   <$> getIndex    <*> getTParam
    4 -> TGen   <$> getIndex    <*> getTParam
    _ -> fail ("Invalid tag: " ++ show tag)

isTVar :: Type -> Bool
isTVar TVar{} = True
isTVar _      = False

instance Pretty Type where
  pp _ (TCon n)       = ppr n
  pp _ (TVar _ m)     = ppr m
  pp _ (TGen _ m)     = ppr m
  pp p (TApp a b)     = optParens (p > 1) (ppr a <+> pp 2 b)
  pp p (TInfix c a b) = optParens (p > 0) (pp 2 a <+> ppr c <+> pp 2 b)

data TParam = TParam
  { paramName :: String
  , paramKind :: Kind
  } deriving (Eq,Show,Ord)

instance Pretty TParam where
  pp _ p = text (paramName p)

putTParam :: Putter TParam
putTParam p = put (paramName p) >> putKind (paramKind p)

getTParam :: Get TParam
getTParam  = TParam <$> get <*> getKind

-- | Type-application introduction.
tapp :: Type -> Type -> Type
tapp  = TApp

-- | Arrow introduction.
tarrow :: Type -> Type -> Type
tarrow  = TInfix arrowConstr
infixr 9 `tarrow`

arrowConstr :: QualName
arrowConstr  = primName "->"

destInfix :: Type -> Maybe (QualName,Type,Type)
destInfix (TInfix qn l r) = return (qn,l,r)
destInfix _               = Nothing

destArrow :: Type -> Maybe (Type,Type)
destArrow ty = do
  (qn,l,r) <- destInfix ty
  guard (qn == arrowConstr)
  return (l,r)

-- | Count the number of arguments to a function.
typeArity :: Type -> Int
typeArity ty = maybe 0 rec (destArrow ty)
  where
  rec (_,r) = 1 + typeArity r

type Kind = Type

putKind :: Putter Kind
putKind  = putType

getKind :: Get Kind
getKind  = getType

-- | The kind of types.
kstar :: Kind
kstar  = TCon (primName "*")

-- | The kind of type constructors.
karrow :: Kind -> Kind -> Kind
karrow  = TInfix arrowConstr
infixr 9 `karrow`

type Sort = Type

setSort :: Sort
setSort = TCon (primName "Set")


-- | Things with quantified variables.
data Forall a = Forall [TParam] a
    deriving (Show,Eq,Ord)

putForall :: Putter a -> Putter (Forall a)
putForall p (Forall ps a) = putListOf putTParam ps >> p a

getForall :: Get a -> Get (Forall a)
getForall a = Forall <$> getListOf getTParam <*> a

forallParams :: Forall a -> [TParam]
forallParams (Forall ps _) = ps

forallData :: Forall a -> a
forallData (Forall _ a) = a

instance Pretty a => Pretty (Forall a) where
  pp _ (Forall ps a) = vars <+> pp 0 a
    where
    vars | null ps   = empty
         | otherwise = text "forall" <+> ppList 0 ps <> char '.'
