module EvilGoateeConst where

import Test.QuickCheck

newtype EvilGoateeConst a b = GoatyConst b
  deriving (Eq, Show)
  
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary 