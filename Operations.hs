module Operations where

dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy _ 0 = Nothing
dividedBy num denom = Just $ go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise =
        go (n - d) d (count + 1)

sumUntil :: (Eq a, Num a) => a -> a
sumUntil n = go n 0
  where
    go n acc
      | n == 0 = acc
      | otherwise =
        go (n - 1) (acc + n)

multipliedBy :: Integral a => a -> a -> a
multipliedBy x y = go x y 0
  where
    go multiplicand multiplier acc
      | multiplier == 0 = acc
      | otherwise =
        go multiplicand (multiplier - 1) (acc + x)

mc91 :: Integral a => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = x
