module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine" 

digits :: Int -> [Int]
digits = map readChar . show
    where
        readChar x = read [x] :: Int

wordNumber :: Int -> String
wordNumber = intercalate "-" .  map digitToWord . digits
