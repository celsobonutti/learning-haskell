module Upper where

import Data.Char (isUpper, toUpper)

filterUpper :: String -> String
filterUpper = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (fst : rest) = toUpper fst : rest

uppercase :: String -> String
uppercase [] = []
uppercase (fst : rest) = toUpper fst : uppercase rest

capitalizedFirst :: String -> Char
capitalizedFirst = toUpper . head
