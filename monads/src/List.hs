module List where

import Control.Monad (ap, liftM)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> ys = ys
  (Cons x xs) <> ys = Cons x $ xs <> ys

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap = liftM

instance Applicative List where
  pure = return
  (<*>) = ap

instance Monad List where
  return a = Cons a mempty
  Nil >>= _ = mempty
  (Cons x tail) >>= f = f x <> (tail >>= f)

generateCons :: Arbitrary a => Gen (List a)
generateCons = do
  a <- arbitrary
  Cons a <$> arbitrary

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency
      [ (1, return Nil),
        (2, generateCons)
      ]

instance Eq a => EqProp (List a) where
  (=-=) = eq
