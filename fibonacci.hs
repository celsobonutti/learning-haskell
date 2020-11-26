module Fibonacci where

calculate :: Integral a => a -> a
calculate n =
  fst $ step n (0, 1)

step :: Integral a => a -> (a, a) -> (a, a)
step n (fstT, sndT) =
  case n of
    0 -> (fstT, sndT)
    m -> step (m - 1) (sndT, fstT + sndT)