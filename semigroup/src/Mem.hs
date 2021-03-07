{-# LANGUAGE TupleSections #-}

module Mem where

import Test.QuickCheck

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

combineMems f g x = (a <> c, d)
  where
    (a, b) = g x
    (c, d) = f b

instance (Semigroup a) => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ combineMems f g

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (mempty,)

type Assoc s a = Mem s a -> Mem s a -> Mem s a -> Bool
