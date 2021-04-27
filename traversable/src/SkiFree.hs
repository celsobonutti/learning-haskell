{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance
  ( Functor n,
    Arbitrary (n a),
    Arbitrary a
  ) =>
  Arbitrary (S n a)
  where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance
  ( Applicative n,
    Testable (n Property),
    EqProp a
  ) =>
  EqProp (S n a)
  where
  (S x y) =-= (S p q) =
    property ((=-=) <$> x <*> p) .&. (y =-= q)

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (fmap f x) (f y)

instance Foldable n => Foldable (S n) where
  foldMap f (S x y) = foldMap f x <> f y

instance Traversable n => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y
