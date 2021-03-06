module Or where

import Test.QuickCheck

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  x@(Snd _) <> _ = x
  _ <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    frequency
      [ (1, Fst <$> arbitrary)
      , (1, Snd <$> arbitrary)
      ]

type GenFirstIntChar = Gen (Or Int Char)

prop_or :: Or Int Char -> Or Int Char -> Bool
prop_or x@(Snd _) y = x <> y == x
prop_or x y = x <> y == y

type Assoc a b = Or a b -> Or a b -> Or a b -> Bool
