module EitherUtils where

lefts' :: [Either a b] -> [a]
lefts' = foldr go []
 where
  go (Left x) acc =
    x : acc
  go (Right y) acc =
    acc

rights' :: [Either a b] -> [b]
rights' = foldr go []
 where
  go (Left x) acc =
    acc
  go (Right y) acc =
    y : acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = (lefts' x, rights' x)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just . f $ x
eitherMaybe' _ (Left _) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right y) = g y

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)