module Two where

data Two a b
  = Two a b

instance Foldable (Two a) where
  foldr f y (Two _ x) = f x y
