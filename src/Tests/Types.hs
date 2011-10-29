module Test.Types where

import Tests.QualName
import Tests.Utils (reduce)

import Pretty
import TypeChecker.Types
import TypeChecker.Unify

import Control.Applicative (pure,(<$>),(<*>))
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import qualified Data.Set as Set


instance Arbitrary Type where
  arbitrary = frequency
    [ (1, TApp   <$> arbitrary        <*> arbitrary)
    , (2, TInfix <$> namespace symbol <*> arbitrary <*> arbitrary)
    , (4, TCon   <$> arbitrary)
    , (4, TVar   <$> arbitrary)
    ]

-- | This instance only ever generates unbound variables.
instance Arbitrary TVar where
  arbitrary = UVar <$> arbitrary

instance Arbitrary TParam where
  arbitrary = TParam
          <$> arbitrary
          <*> pure True
          <*> ident
          <*> arbitraryKind

arbitraryKind :: Gen Kind
arbitraryKind  = oneof
  [ pure kstar
  , karrow <$> arbitraryKind <*> arbitraryKind
  ]

arbitraryScheme :: Gen Scheme
arbitraryScheme  = do
  ty <- arbitrary :: Gen Type
  let vars = Set.toList (typeVars ty)
  keep <- reduce vars
  return (quantify keep ty)
