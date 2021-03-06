module BoolConj where

import Test.QuickCheck

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False
  _                <> x = x

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type Assoc = BoolConj -> BoolConj -> BoolConj -> Bool