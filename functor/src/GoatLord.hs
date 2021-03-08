module GoatLord where

import Test.QuickCheck

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats
      (GoatLord a)
      (GoatLord a)
      (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (f <$> x) (f <$> y) (f <$> z)

genMoreGoats :: (Arbitrary a) => Gen (GoatLord a)
genMoreGoats = do
  a <- arbitrary
  b <- arbitrary
  MoreGoats a b <$> arbitrary

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary =
    frequency
      [ (1, return NoGoat)
      , (2, OneGoat <$> arbitrary)
      , (2, genMoreGoats)
      ]