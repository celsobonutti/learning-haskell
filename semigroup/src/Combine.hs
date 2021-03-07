module Combine where

import Test.QuickCheck

newtype Combine a b = Combine {unCombine :: a -> b}

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

instance Show (Combine a b) where
  show _ = "Combine a b"
