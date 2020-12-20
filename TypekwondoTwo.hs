import Prelude

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = b == aToB a

arith ::
  Num b =>
  (a -> b) ->
  Integer ->
  a ->
  b
arith aToB int =
  aToB

mTh :: Num a => a -> a -> a -> a
mTh x y z = x * y * z

n = mTh 3

t = \n -> case odd n of
  True -> f n
  False -> n
  where
    f n = n + 1

t' = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x