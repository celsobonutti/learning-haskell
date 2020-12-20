module Lists where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail a =
    case a of
        [] -> Nothing
        [_] -> Nothing
        _ : xs -> Just xs

eftBool :: Bool -> Bool -> [Bool]
eftBool st en
        | st > en
            = []
        | st == en
            = [en]
        | st
            = [st]
        | not st
            = st : eftBool (not st) en 

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd st en
        | st > en
            = []
        | otherwise
            = takeWhile (/= en) (iterate succ st) ++ [en]

eftInt :: Int -> Int -> [Int]
eftInt st en 
        | st > en
            = []
        | otherwise
            = takeWhile (/= succ en) (iterate succ st)


eftChar :: Char -> Char -> [Char]
eftChar st en 
        | st > en
            = []
        | otherwise
            = takeWhile (/= succ en) (iterate succ st)
           
