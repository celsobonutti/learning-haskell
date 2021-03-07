module Quant where

data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance
