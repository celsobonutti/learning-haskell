module Four where

import Test.QuickCheck (Arbitrary (arbitrary))

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d)
  where
  (Four w x y z) <> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

type Assoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool