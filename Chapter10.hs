module Chapter10 where

stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

combinations :: [(Char, Char, Char)]
combinations = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

combinationsWithP :: [(Char, Char, Char)]
combinationsWithP = [(s1, v, s2) | s1 <- stops, s1 == 'p', v <- vowels, s2 <- stops]

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\i acc -> f i || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem el = foldr (\i acc -> el == i || acc) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' el = myAny (== el)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\i acc -> (f i) : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\i acc -> if f i then i : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\i acc -> f i ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\i acc -> if f i acc == GT then i else acc)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\i acc -> if f i acc == LT then i else acc)