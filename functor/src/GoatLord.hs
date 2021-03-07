module GoatLord where

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats
      (GoatLord a)
      (GoatLord a)
      (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (f <$> x) (f <$> y) (f <$> z)
