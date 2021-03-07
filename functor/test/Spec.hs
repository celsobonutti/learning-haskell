{-# LANGUAGE TemplateHaskell #-}

import Flip
import Four
import Identity
import K
import Pair
import Possibly
import Quant
import Sum
import TalkToMe
import Test.QuickCheck (quickCheckAll)
import Test.QuickCheck.Function
import Three
import Two
import Wrap

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  ((g . f) <$> x) == (g <$> (f <$> x))

idIdentity :: Identity String -> Bool
idIdentity = functorIdentity

idCompose :: Identity String -> Fun String Int -> Fun Int Char -> Bool
idCompose = functorCompose

pairIdentity :: Identity Int -> Bool
pairIdentity = functorIdentity

pairCompose :: Pair Int -> Fun Int Char -> Fun Char Float -> Bool
pairCompose = functorCompose

twoIdentity :: Two Int Bool -> Bool
twoIdentity = functorIdentity

twoCompose :: Two Int Bool -> Fun Bool Char -> Fun Char String -> Bool
twoCompose = functorCompose

threeIdentity :: Three Int Bool Char -> Bool
threeIdentity = functorIdentity

threeCompose :: Three Int Bool Char -> Fun Char String -> Fun String Bool -> Bool
threeCompose = functorCompose

threeIdentity' :: Three' Int Bool -> Bool
threeIdentity' = functorIdentity

threeCompose' :: Three' Int Bool -> Fun Bool String -> Fun String Integer -> Bool
threeCompose' = functorCompose

fourIdentity :: Four Int Bool Char Float -> Bool
fourIdentity = functorIdentity

fourCompose :: Four Int Bool Char Float -> Fun Float String -> Fun String Bool -> Bool
fourCompose = functorCompose

fourIdentity' :: Four' String Bool -> Bool
fourIdentity' = functorIdentity

fourCompose' :: Four' String Bool -> Fun Bool Float -> Fun Float String -> Bool
fourCompose' = functorCompose

possiblyIdentity :: Possibly Int -> Bool
possiblyIdentity = functorIdentity

possiblyCompose :: Possibly Int -> Fun Int Float -> Fun Float String -> Bool
possiblyCompose = functorCompose

sumIdentity :: Sum Int Float -> Bool
sumIdentity = functorIdentity

sumCompose :: Sum Int Float -> Fun Float String -> Fun String Bool -> Bool
sumCompose = functorCompose

wrapIdentity :: Wrap Maybe Int -> Bool
wrapIdentity = functorIdentity

wrapCompose :: Wrap (Either Int) Float -> Fun Float String -> Fun String Bool -> Bool
wrapCompose = functorCompose

quantIdentity :: Quant Int Float -> Bool
quantIdentity = functorIdentity

quantCompose :: Quant Int Float -> Fun Float String -> Fun String Char -> Bool
quantCompose = functorCompose

kIdentity :: K Int Char -> Bool
kIdentity = functorIdentity

kCompose :: K Int Char -> Fun Char String -> Fun String Bool -> Bool
kCompose = functorCompose

flipIdentity :: Flip L String Char -> Bool
flipIdentity = functorIdentity

flipCompose :: Flip L String Char -> Fun Char String -> Fun String Bool -> Bool
flipCompose = functorCompose

return []

check = $quickCheckAll

main :: IO Bool
main = check
