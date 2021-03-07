module Two where

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)
