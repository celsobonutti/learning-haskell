module Fibonacci where

calculate :: Integral a => a -> a
calculate =
  fst . step (0, 1)

step :: Integral a => (a, a) -> a -> (a, a)
step (fstT, sndT) n
  | n == 0 = (fstT, sndT)
  | n > 0 = step (sndT, fstT + sndT) (n - 1)
  | n < 0 = (0, 0)