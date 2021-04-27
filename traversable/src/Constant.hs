module Constant where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldr _ x _ = x

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure (Constant x)

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary
