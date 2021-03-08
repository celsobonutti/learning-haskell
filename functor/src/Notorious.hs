module Notorious where

import Test.QuickCheck

data Notorious g o a t
  = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious x y g) = Notorious x y $ f <$> g

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => Arbitrary (Notorious g o a t) where
  arbitrary = do
    o <- arbitrary 
    a <- arbitrary 
    Notorious o a <$> arbitrary 