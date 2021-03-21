module Four where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four a b = Four a a a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Four a b) where
  (Four w x y z) <> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b) => Monoid (Four a b) where
  mempty = Four mempty mempty mempty mempty

instance Monoid a => Applicative (Four a) where
  pure = Four mempty mempty mempty
  (Four w x y fz) <*> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (fz z')

instance Monoid a => Functor (Four a) where
  fmap f x = pure f <*> x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four a b) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four a b) where
  (=-=) = eq
