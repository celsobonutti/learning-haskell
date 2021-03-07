module Pair where

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
