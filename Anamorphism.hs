module Anamorphism where

import Data.Maybe (fromMaybe)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Nothing -> []
    Just (y, next) -> y : myUnfoldr f next

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr go
 where
  go y = Just (y, f y)