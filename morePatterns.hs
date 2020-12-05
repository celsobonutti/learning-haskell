tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
  where
    (xLast, _) = x `divMod` 100
    (_, d) = xLast `divMod` 100

foldBool :: a -> a -> Bool -> a
foldBool x y bool
  | bool = x
  | not bool = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
