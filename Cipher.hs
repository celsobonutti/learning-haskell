module Cipher where

import Data.Char

caesar :: Char -> Int -> Char
caesar x i = ciphered
  where
    upper = isUpper x
    numeric = ord x
    base = if upper then 64 else 97
    round = numeric - base
    ciphered = chr $ mod (round + i) 26 + base

unCaesar :: Char -> Int -> Char
unCaesar x i = caesar x (- i)
