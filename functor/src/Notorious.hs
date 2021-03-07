module Notorious where

data Notorious g o a t
  = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious x y g) = Notorious x y $ f <$> g
