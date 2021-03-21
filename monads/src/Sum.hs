module Sum where

import Control.Applicative (liftA)
import Control.Monad (ap, liftM)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap = liftA

instance Applicative (Sum a) where
  pure = return
  (<*>) = ap

instance Monad (Sum a) where
  return = Second
  (First x) >>= _ = First x
  (Second y) >>= f = f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary =
    frequency
      [ (1, First <$> arbitrary),
        (1, Second <$> arbitrary)
      ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq
