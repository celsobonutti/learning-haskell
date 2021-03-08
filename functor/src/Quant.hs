module Quant where

import Test.QuickCheck

data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary =
    frequency
      [ (1, return Finance)
      , (1, Desk <$> arbitrary)
      , (1, Bloor <$> arbitrary)
      ]