module LiftItOut where

newtype LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut f) = LiftItOut $ g <$> f
