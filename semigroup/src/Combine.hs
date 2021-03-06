module Combine where

import Test.QuickCheck

newtype Combine a b = Combine {unCombine :: a -> b}

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary