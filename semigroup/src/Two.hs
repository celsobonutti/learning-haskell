module Two where

import Test.QuickCheck (Arbitrary(arbitrary))

data Two a b = Two a b
  deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b) =>
  Semigroup (Two a b)
  where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    Two a <$> arbitrary

type Assoc a b = Two a b -> Two a b -> Two a b -> Bool