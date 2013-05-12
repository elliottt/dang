{-# LANGUAGE Trustworthy #-}

module Dang.ModuleSystem.Types where

import Dang.ModuleSystem.QualName (QualName,simpleName,qualSymbol,putQualName,getQualName)
import Dang.TypeChecker.Vars (TParam,getTParam,putTParam,TVar,getTVar,putTVar)

import Control.Applicative (Applicative(..),(<$>))
import Data.Serialize (Get,Putter,getWord8,putWord8)


-- Name Uses -------------------------------------------------------------------

data UsedName
  = UsedType QualName
  | UsedTerm QualName
    deriving (Ord,Show,Eq)

mapUsedName :: (QualName -> QualName) -> (UsedName -> UsedName)
mapUsedName f (UsedType qn) = UsedType (f qn)
mapUsedName f (UsedTerm qn) = UsedTerm (f qn)

simpleUsedName :: UsedName -> UsedName
simpleUsedName  = mapUsedName (simpleName . qualSymbol)

usedQualName :: UsedName -> QualName
usedQualName (UsedType qn) = qn
usedQualName (UsedTerm qn) = qn


-- Type Representation ---------------------------------------------------------

data IfaceType
  = IfaceTVar (TVar IfaceKind)
  | IfaceTCon QualName
  | IfaceTApp IfaceType IfaceType
  | IfaceForall (TParam IfaceKind) IfaceType

putIfaceType :: Putter IfaceType
putIfaceType ty = case ty of
  IfaceTVar n     -> putWord8 0 >> putTVar putIfaceKind n
  IfaceTCon qn    -> putWord8 1 >> putQualName qn
  IfaceTApp l r   -> putWord8 2 >> putIfaceType l           >> putIfaceType r
  IfaceForall p b -> putWord8 3 >> putTParam putIfaceKind p >> putIfaceType b

getIfaceType :: Get IfaceType
getIfaceType  = getWord8 >>= \ tag -> case tag of
  0 -> IfaceTVar   <$> getTVar getIfaceKind
  1 -> IfaceTCon   <$> getQualName
  2 -> IfaceTApp   <$> getIfaceType           <*> getIfaceType
  3 -> IfaceForall <$> getTParam getIfaceKind <*> getIfaceType
  _ -> fail ("unknown IfaceType tag: " ++ show tag)

type IfaceKind = IfaceType

putIfaceKind :: Putter IfaceKind
putIfaceKind  = putIfaceType

getIfaceKind :: Get IfaceKind
getIfaceKind  = getIfaceType
