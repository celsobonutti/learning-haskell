module Wrap where

import Test.QuickCheck
import Test.QuickCheck.Function

newtype Wrap f a
  = Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap g) = Wrap $ f <$> g

instance Arbitrary (f a) => Arbitrary (Wrap f a) where
  arbitrary = Wrap <$> arbitrary