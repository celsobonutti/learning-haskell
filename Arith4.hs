-- arith4.hs
module Main where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print x
  where
    x = roundTrip 4 :: Integer
