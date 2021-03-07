module HeavyLifting where

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

a :: [Int]
a = (+ 1) <$> read "[1]"

b :: Maybe [[Char]]
b = (++ "lol") <$$> Just ["Hi,", "Hello"]

c :: Integer -> Integer
c = (* 2) <$> (\x -> x - 2)

d :: Integer -> [Char]
d =
  (return '1' ++) . show <$> (\x -> [x, 1 .. 3])

e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = read . ("123" ++) . show <$> ioi
   in (* 3) <$> changed