module TalkToMe where

import Test.QuickCheck

data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str (f x)
  fmap g (Read f) = Read (g <$> f)

instance Show a => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print x y) = x ++ show y
  show (Read _) = show "Read f"

instance Eq a => Eq (TalkToMe a) where
  Halt == Halt = True
  (Print x y) == (Print x' y') = x == x' && y == y'
  (Read _) == (Read _) = False
  _ == _ = False

generatePrint :: (Arbitrary a) => Gen (TalkToMe a)
generatePrint = do
  str <- arbitrary
  Print str <$> arbitrary

instance (Arbitrary a) => Arbitrary (TalkToMe a) where
  arbitrary =
    frequency
      [ (1, return Halt),
        (2, generatePrint),
        (2, Read <$> arbitrary)
      ]
