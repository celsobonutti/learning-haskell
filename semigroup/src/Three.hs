module Three where

import Test.QuickCheck (Arbitrary (arbitrary))

data Three a b c = Three a b c
  deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c)
  where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

type Assoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool