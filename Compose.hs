{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Compose where

import Control.Applicative

newtype Compose f g a = Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ (pure . pure) x

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ (<*>) <$> f <*> a
    where
      test = (<*>) <$> f

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap fun (Compose f) = (foldMap . foldMap) fun f 

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse fun (Compose f) =
    Compose <$> (traverse . traverse) fun f
