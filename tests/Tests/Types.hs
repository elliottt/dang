{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Types where

import QualName (qualName)
import Tests.QualName (namespace,symbol,ident,conident)
import Tests.Utils (reduce)
import TypeChecker.Types
import TypeChecker.Unify

import Control.Applicative (pure,(<$>),(<*>))
import Test.QuickCheck
import qualified Data.Set as Set


-- | This instance only ever generates unbound variables.
instance Arbitrary TVar where
  arbitrary = UVar <$> arbitrary

instance Arbitrary TParam where
  arbitrary = TParam
          <$> arbitrary
          <*> pure True
          <*> ident
          <*> arbitraryKind

-- | Base and recursive cases, depending on the size parameter.
inductive :: Gen a -> Gen a -> Gen a
inductive base rec = sized $ \ n -> case n of
  0 -> base
  _ -> resize (n-1) $ frequency
    [ (2,base)
    , (1,rec)
    ]

arbitraryKind :: Gen Kind
arbitraryKind  = inductive base rec
  where
  base = oneof [ return kstar ]
  rec  = karrow <$> arbitraryKind <*> arbitraryKind

monoType :: Gen Type
monoType  = inductive base rec
  where
  base = oneof
    [ TVar <$> arbitrary
    , TCon <$> namespace qualName conident
    ]
  rec = frequency
    [ (2, tarrow <$> monoType                  <*> monoType)
    , (1, TApp   <$> monoType                  <*> monoType)
    , (1, TInfix <$> namespace qualName symbol <*> monoType <*> monoType)
    ]

scheme :: Gen Scheme
scheme  = sized $ \ n -> do
  fun <- resize (n-1) monoType
  ps  <- reduce (Set.toList (typeVars fun))
  return (quantify ps (toQual fun))
