module K where

import Test.QuickCheck

newtype K a b = K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary 