module Identity where

import Test.QuickCheck ( Arbitrary(arbitrary) )

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type Assoc a = Identity a -> Identity a -> Identity a -> Bool