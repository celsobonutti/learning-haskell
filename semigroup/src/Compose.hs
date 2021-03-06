module Compose where

import Test.QuickCheck

newtype Compose a = Compose {unCompose :: a -> a}

instance Semigroup (Compose a) where
  Compose f <> Compose g = Compose $ g . f

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Compose a) where
  arbitrary = Compose <$> arbitrary