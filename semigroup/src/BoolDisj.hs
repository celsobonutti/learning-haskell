module BoolDisj where

import Test.QuickCheck ( Arbitrary(arbitrary) )

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _                <> x = x

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type Assoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool