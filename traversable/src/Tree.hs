module Tree where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show, Ord)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node left x right) = foldMap f left <> f x <> foldMap f right

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node left x right) = Node <$> traverse f left <*> f x <*> traverse f right

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency
      [ (1, return Empty),
        (2, Leaf <$> arbitrary),
        (2, liftA3 Node arbitrary arbitrary arbitrary)
      ]
