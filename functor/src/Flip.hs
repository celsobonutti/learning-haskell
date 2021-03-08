{-# LANGUAGE FlexibleInstances #-}

module Flip where

import Test.QuickCheck ( Arbitrary(arbitrary) )

newtype Flip f a b
  = Flip (f b a)
  deriving (Eq, Show)

newtype L a b
  = L a
  deriving (Eq, Show)

instance Functor (Flip L a) where
  fmap f (Flip (L a)) = Flip $ L (f a)

instance Arbitrary b => Arbitrary (Flip L a b) where
  arbitrary = Flip . L <$> arbitrary
