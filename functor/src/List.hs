module List where

import Test.QuickCheck

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons head tail) = Cons (f head) (f <$> tail)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    frequency
      [ (1, return Nil)
      ,
        ( 3
        , do
            a <- arbitrary
            Cons a <$> arbitrary
        )
      ]