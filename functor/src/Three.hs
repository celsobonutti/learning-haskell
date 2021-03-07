module Three where

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)
