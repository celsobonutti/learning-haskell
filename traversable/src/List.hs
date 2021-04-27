module List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show, Ord)

append :: List a -> a -> List a
append = undefined

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons head tail) = Cons (f head) (fmap f tail)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons head tail) = f head <> foldMap f tail

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons head tail) = Cons <$> f head <*> traverse f tail

data Three a b c
  = Three a b c

instance Functor (Three a b) where
  fmap = undefined

instance Foldable (Three a b) where
  foldMap = undefined

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency
      [ (1, return Nil),
        (2, Cons <$> arbitrary <*> arbitrary)
      ]
