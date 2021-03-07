module IgnoreOne where

data IgnoreOne f g a b
  = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething f g) = IgnoringSomething f $ h <$> g
