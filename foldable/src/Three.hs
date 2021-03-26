module Three where

data Three a b c
  = Three a b c

instance Foldable (Three a b) where
  foldr f y (Three _ _ x) = f x y

data Three' a b
  = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ x y) = f x <> f y
