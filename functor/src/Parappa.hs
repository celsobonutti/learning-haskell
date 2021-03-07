module Parappa where

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa f g) = DaWrappa (h <$> f) (h <$> g)
