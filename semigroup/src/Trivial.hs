module Trivial where

import Test.QuickCheck (Arbitrary (arbitrary))

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Monoid Trivial where
  mempty = Trivial

type Assoc =
  Trivial -> Trivial -> Trivial -> Bool

type Identity =
  Trivial -> Bool
