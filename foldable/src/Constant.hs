module Constant where

newtype Constant a b
  = Constant b

instance Foldable (Constant a) where
  foldr f y (Constant x) = f x y
