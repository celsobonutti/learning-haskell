{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

first = (* 9) 6

second = head [(0, "doge"), (1, "kitteh")]

third = head [(0 :: Integer, "doge"), (1, "kitteh")]

fourth = if False then True else False

fifth = length [1, 2, 3, 4, 5]

sixth = (length [1, 2, 3, 4]) > (length "TACOCAT")

takeFirst :: [a] -> a
takeFirst (x : _) = x

functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

functionS :: (a, b) -> b
functionS (_x, y) = y

i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x

c'' :: b -> a -> b
c'' x _ = x

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r a = reverse a

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC . aToB $ a

a :: (a -> c) -> a -> a
a _ y = y

a' :: (a -> b) -> a -> b
a' x y = x y