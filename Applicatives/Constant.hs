module Constant where

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure = Constant . mempty
  (Constant f) <*> (Constant g) = Constant $ f <> g
