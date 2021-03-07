{-# LANGUAGE TupleSections #-}

module Mem where

import Test.QuickCheck

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance (Semigroup a) => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ \x ->
    let (a, b) = f x
        (c, d) = g b
     in (a <> c, d)

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem (mempty,)

instance
  (CoArbitrary s, Arbitrary a, Arbitrary s) =>
  Arbitrary (Mem s a)
  where
  arbitrary = Mem <$> arbitrary

instance Show (Mem s a) where
  show _ = "Mem s a"