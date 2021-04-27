module RandomExample where

import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

randomDice = randomR (1, 6)

rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomDice s
      (d2, s2) = randomDice s1
      (d3, _) = randomDice s2
  (intToDie d1, intToDie d2, intToDie d3)

