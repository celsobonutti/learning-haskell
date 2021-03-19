module Validation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a
  = Failure' [e]
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' s) = Success' $ f s

instance (Monoid e) => Applicative (Validation e) where
  pure = Success'
  Failure' e <*> Failure' e' = Failure' $ e ++ e'
  Failure' e <*> _ = Failure' e
  _ <*> Failure' e = Failure' e
  Success' s <*> Success' s' = Success' $ s s'

instance (Arbitrary a, Arbitrary e, CoArbitrary a, CoArbitrary e) => Arbitrary (Validation a e) where
  arbitrary =
    frequency
      [ (1, Success' <$> arbitrary),
        (1, Success' <$> arbitrary)
      ]

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq
