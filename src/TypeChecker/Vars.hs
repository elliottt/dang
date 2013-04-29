{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module TypeChecker.Vars where

import Dang.Pretty
import QualName
import Variables

import Control.Applicative (Applicative(..),(<$>))
import Data.Data (Data)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Serialize
    (Get,Putter,get,put,getWord8,putWord8,putWord32be,getWord32be)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax (Lift(..),liftString)
import qualified Data.Set as Set


-- Type Indices ----------------------------------------------------------------

type Index = Int

putIndex :: Putter Index
putIndex  = putWord32be . toEnum

getIndex :: Get Index
getIndex  = fromEnum <$> getWord32be


-- Type Variables --------------------------------------------------------------

data TVar kind
  = GVar (TParam kind)
  | UVar (TParam kind)
    deriving (Show,Eq,Ord,Data,Typeable)

instance Lift kind => Lift (TVar kind) where
  lift tv = case tv of
    UVar p -> [| UVar $(lift p) |]
    GVar p -> [| GVar $(lift p) |]

putTVar :: Putter kind -> Putter (TVar kind)
putTVar putKind tv = case tv of
  GVar p -> putWord8 0 >> putTParam putKind p
  UVar p -> putWord8 1 >> putTParam putKind p

getTVar :: Get kind -> Get (TVar kind)
getTVar getKind = getWord8 >>= \ tag -> case tag of
  0 -> GVar <$> getTParam getKind
  1 -> UVar <$> getTParam getKind
  _ -> fail ("Invalid TVar tag: " ++ show tag)

instance Pretty (TVar kind) where
  pp p (GVar v) = pp p v
  pp p (UVar v) = pp p v

instance FreeVars (TVar kind) where
  freeVars (UVar v) = Set.singleton (simpleName (paramName v))
  freeVars GVar{}   = Set.empty


-- Type Parameters -------------------------------------------------------------

data TParam kind = TParam
  { paramIndex      :: Index
  , paramFromSource :: Bool
  , paramName       :: String
  , paramKind       :: kind
  } deriving (Show,Data,Typeable)

instance Lift kind => Lift (TParam kind) where
  lift tp = [| TParam
    { paramIndex      = $(lift       (paramIndex tp))
    , paramFromSource = $(lift       (paramFromSource tp))
    , paramName       = $(liftString (paramName tp))
    , paramKind       = $(lift       (paramKind tp))
    } |]

instance Eq (TParam kind) where
  (==) = (==) `on` paramIndex
  (/=) = (/=) `on` paramIndex

instance Ord (TParam kind) where
  compare = comparing paramIndex

instance FreeVars (TParam kind) where
  freeVars = Set.singleton . simpleName . paramName

instance Pretty (TParam kind) where
  pp _ p = text (paramName p)

modifyTParamIndex :: (Index -> Index) -> (TParam kind -> TParam kind)
modifyTParamIndex f p = p { paramIndex = f (paramIndex p) }

setTParamIndex :: Index -> TParam kind -> TParam kind
setTParamIndex ix p = p { paramIndex = ix }

putTParam :: Putter kind -> Putter (TParam kind)
putTParam putKind p = putIndex (paramIndex p)
                   >> put (paramFromSource p)
                   >> put (paramName p)
                   >> putKind (paramKind p)

getTParam :: Get kind -> Get (TParam kind)
getTParam getKind = TParam <$> getIndex <*> get <*> get <*> getKind
