module IgnoreOne where

import Test.QuickCheck

data IgnoreOne f g a b
  = IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething f g) = IgnoringSomething f $ h <$> g

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = do
    f <- arbitrary
    IgnoringSomething f <$> arbitrary