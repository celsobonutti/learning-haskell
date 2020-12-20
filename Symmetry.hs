module Symmetry where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen
    ++ thirdSen ++ fourthSen 

myWords :: String -> [String]
myWords =
    chopOn ' '

myLines:: String -> [String]
myLines =
    chopOn '\n' 

chopOn:: Char -> String -> [String]
chopOn char text =
    let
        start = takeWhile (/= char) text
        rest = drop 1 . dropWhile (/= char) $ text
    in
        if char `elem` rest then
            start : chopOn char rest
        else
            [start, rest]
