module Four where

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y $ f z

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y $ f z
