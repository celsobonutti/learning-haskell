module LibraryFunctions where

import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr isPresent False
  where
    isPresent _ True = True
    isPresent y False = x == y

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr min' Nothing
  where
    min' x Nothing = Just x
    min' x (Just y) = Just $ min x y

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr max' Nothing
  where
    max' x Nothing = Just x
    max' x (Just y) = Just $ max x y

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ x -> x + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\el acc -> f el <> acc) mempty

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
