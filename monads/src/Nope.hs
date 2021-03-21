module Nope where

import Control.Monad (ap, liftM)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a
  = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap = liftM

instance Applicative Nope where
  pure = return
  (<*>) = ap

instance Monad Nope where
  return _ = NopeDotJpg
  f >>= _ = NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg
