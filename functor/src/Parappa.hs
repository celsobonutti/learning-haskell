module Parappa where

import Test.QuickCheck

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa f g) = DaWrappa (h <$> f) (h <$> g)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = do
    f <- arbitrary
    DaWrappa f <$> arbitrary 