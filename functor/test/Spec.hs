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

prop_idIdentity :: Identity String -> Bool
prop_idIdentity = functorIdentity

prop_idCompose :: Identity String -> Fun String Int -> Fun Int Char -> Bool
prop_idCompose = functorCompose

prop_pairIdentity :: Identity Int -> Bool
prop_pairIdentity = functorIdentity

prop_pairCompose :: Pair Int -> Fun Int Char -> Fun Char Float -> Bool
prop_pairCompose = functorCompose

prop_twoIdentity :: Two Int Bool -> Bool
prop_twoIdentity = functorIdentity

prop_twoCompose :: Two Int Bool -> Fun Bool Char -> Fun Char String -> Bool
prop_twoCompose = functorCompose

prop_threeIdentity :: Three Int Bool Char -> Bool
prop_threeIdentity = functorIdentity

prop_threeCompose :: Three Int Bool Char -> Fun Char String -> Fun String Bool -> Bool
prop_threeCompose = functorCompose

prop_threeIdentity' :: Three' Int Bool -> Bool
prop_threeIdentity' = functorIdentity

prop_threeCompose' :: Three' Int Bool -> Fun Bool String -> Fun String Integer -> Bool
prop_threeCompose' = functorCompose

prop_fourIdentity :: Four Int Bool Char Float -> Bool
prop_fourIdentity = functorIdentity

prop_fourCompose :: Four Int Bool Char Float -> Fun Float String -> Fun String Bool -> Bool
prop_fourCompose = functorCompose

prop_fourIdentity' :: Four' String Bool -> Bool
prop_fourIdentity' = functorIdentity

prop_fourCompose' :: Four' String Bool -> Fun Bool Float -> Fun Float String -> Bool
prop_fourCompose' = functorCompose

prop_possiblyIdentity :: Possibly Int -> Bool
prop_possiblyIdentity = functorIdentity

prop_possiblyCompose :: Possibly Int -> Fun Int Float -> Fun Float String -> Bool
prop_possiblyCompose = functorCompose

prop_sumIdentity :: Sum Int Float -> Bool
prop_sumIdentity = functorIdentity

prop_sumCompose :: Sum Int Float -> Fun Float String -> Fun String Bool -> Bool
prop_sumCompose = functorCompose

prop_wrapIdentity :: Wrap Maybe Int -> Bool
prop_wrapIdentity = functorIdentity

prop_wrapCompose :: Wrap (Either Int) Float -> Fun Float String -> Fun String Bool -> Bool
prop_wrapCompose = functorCompose

prop_quantIdentity :: Quant Int Float -> Bool
prop_quantIdentity = functorIdentity

prop_quantCompose :: Quant Int Float -> Fun Float String -> Fun String Char -> Bool
prop_quantCompose = functorCompose

prop_kIdentity :: K Int Char -> Bool
prop_kIdentity = functorIdentity

prop_kCompose :: K Int Char -> Fun Char String -> Fun String Bool -> Bool
prop_kCompose = functorCompose

prop_talkToMeIdentity :: String -> TalkToMe Char -> Bool
prop_talkToMeIdentity x (Read f) = f x == (id <$> f) x
prop_talkToMeIdentity _ f = functorIdentity f

prop_talkToMeCompose :: String -> TalkToMe Char -> Fun Char String -> Fun String Int -> Bool
prop_talkToMeCompose x (Read f) (Fun _ g) (Fun _ h) = (h . g <$> f) x == (h <$> (g <$> f)) x
prop_talkToMeCompose _ f g h = functorCompose f g h
return []

check = $quickCheckAll

main :: IO Bool
main = check
