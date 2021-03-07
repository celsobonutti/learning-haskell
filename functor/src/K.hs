module K where

data K a b = K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a
