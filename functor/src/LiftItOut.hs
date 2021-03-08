module LiftItOut where

import Test.QuickCheck

newtype LiftItOut f a = LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut f) = LiftItOut $ g <$> f

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = LiftItOut <$> arbitrary 