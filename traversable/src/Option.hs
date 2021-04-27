module Option where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Option a
  = Some a
  | None
  deriving (Eq, Show, Ord)

instance Functor Option where
  fmap f (Some x) = Some $ f x
  fmap _ None = None

instance Foldable Option where
  foldMap f (Some x) = f x
  foldMap _ None = mempty

instance Traversable Option where
  traverse f (Some x) = Some <$> f x
  traverse _ None = pure None

instance Eq a => EqProp (Option a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Option a) where
  arbitrary =
    frequency
      [ (1, return None),
        (2, Some <$> arbitrary)
      ]
