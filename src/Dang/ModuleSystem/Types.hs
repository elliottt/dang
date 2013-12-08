{-# LANGUAGE Trustworthy #-}

module Dang.ModuleSystem.Types where

import Dang.ModuleSystem.QualName
           (Name,putName,getName)

import Control.Applicative (Applicative(..),(<$>))
import Data.Serialize (Get,Putter,getWord8,putWord8,get,put,getListOf,putListOf)


-- Name Uses -------------------------------------------------------------------

data UsedName
  = UsedType Name
  | UsedTerm Name
    deriving (Ord,Show,Eq)

mapUsedName :: (Name -> Name) -> (UsedName -> UsedName)
mapUsedName f (UsedType qn) = UsedType (f qn)
mapUsedName f (UsedTerm qn) = UsedTerm (f qn)

usedName :: UsedName -> Name
usedName (UsedType qn) = qn
usedName (UsedTerm qn) = qn


-- Type Representation ---------------------------------------------------------

type IfaceProp = IfaceType

data IfaceType
  = IfaceTVar IfaceParam
  | IfaceTCon Name
  | IfaceTApp IfaceType IfaceType
  | IfaceForall [IfaceParam] [IfaceProp] IfaceType

putIfaceType :: Putter IfaceType
putIfaceType ty = case ty of
  IfaceTVar tp        -> putWord8 0 >> putIfaceParam tp
  IfaceTCon qn        -> putWord8 1 >> putName qn
  IfaceTApp l r       -> putWord8 2 >> putIfaceType l   >> putIfaceType r
  IfaceForall tp ps b -> putWord8 3 >> putListOf putIfaceParam tp
                                    >> putListOf putIfaceType ps
                                    >> putIfaceType b

getIfaceType :: Get IfaceType
getIfaceType  = getWord8 >>= \ tag -> case tag of
  0 -> IfaceTVar   <$> getIfaceParam
  1 -> IfaceTCon   <$> getName
  2 -> IfaceTApp   <$> getIfaceType  <*> getIfaceType
  3 -> IfaceForall <$> getListOf getIfaceParam
                   <*> getListOf getIfaceType
                   <*> getIfaceType
  _ -> fail ("unknown IfaceType tag: " ++ show tag)


-- | Interface-written types won't ever have unification variables present,
-- which allows for the parameter type to just contain the index.
data IfaceParam = IfaceParam { ipName  :: Maybe String
                             , ipIndex :: !Int
                             , ipKind  :: IfaceKind }

putIfaceParam :: Putter IfaceParam
putIfaceParam ip = put (ipName ip)
                >> put (ipIndex ip)
                >> putIfaceKind (ipKind ip)

getIfaceParam :: Get IfaceParam
getIfaceParam  = IfaceParam <$> get <*> get <*> getIfaceKind


type IfaceKind = IfaceType

putIfaceKind :: Putter IfaceKind
putIfaceKind  = putIfaceType

getIfaceKind :: Get IfaceKind
getIfaceKind  = getIfaceType
