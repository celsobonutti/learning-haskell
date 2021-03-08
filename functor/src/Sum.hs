module Sum where

import Test.QuickCheck

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary =
    frequency
      [ (1, First <$> arbitrary)
      , (1, Second <$> arbitrary)
      ]