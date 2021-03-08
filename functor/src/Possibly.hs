module Possibly where

import Test.QuickCheck

data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers x) = Yeppers (f x)
  fmap _ LolNope = LolNope

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary =
    frequency
      [ (1, return LolNope)
      , (1, Yeppers <$> arbitrary)
      ]