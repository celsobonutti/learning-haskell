module Validation where

import Test.QuickCheck (Arbitrary (arbitrary), frequency)

data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure x <> Failure y = Failure $ x <> y
  x@(Success _) <> _ = x
  _ <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary =
    frequency
      [ (1, Failure <$> arbitrary)
      , (1, Success <$> arbitrary)
      ]

type Assoc a b = Validation a b -> Validation a b -> Validation a b -> Bool