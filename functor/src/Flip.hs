{-# LANGUAGE FlexibleInstances #-}

module Flip where

import Test.QuickCheck

newtype Flip f a b
  = Flip (f b a)
  deriving (Eq, Show)

newtype L a b
  = L a
  deriving (Eq, Show)

instance Functor (Flip L a) where
  fmap f (Flip (L a)) = Flip $ L (f a)
