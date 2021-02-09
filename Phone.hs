module Phone where

import Data.Char (isUpper, toLower)
import Data.List (elemIndex, elemIndices, find, maximumBy, words)
import Data.Maybe (fromMaybe)

newtype DaPhone = DaPhone [(Char, String)]

phone :: DaPhone
phone =
  DaPhone
    [ ('1', "1")
    , ('2', "abc2")
    , ('3', "def3")
    , ('4', "ghi4")
    , ('5', "jkl5")
    , ('6', "mno6")
    , ('7', "pqrs7")
    , ('8', "tuv8")
    , ('9', "wxyz9")
    , ('*', "*^")
    , ('0', " +_0")
    , ('#', "#.,")
    ]

type Digit = Char

type Presses = Int

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone buttons) digit = keypresses
 where
  button = find (\(_, digits) -> toLower digit `elem` digits) buttons
  keypresses = case button of
    Just (key, digits) ->
      let presses = 1 + fromMaybe 0 (digit `elemIndex` digits)
       in if isUpper digit
            then [('*', 1), (key, presses)]
            else [(key, presses)]
    Nothing ->
      error "Invalid character"

callPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
callPhonesDead phone = foldMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

tupMax :: [(a, Int)] -> (a, Int)
tupMax = maximumBy (\(_, x) (_, y) -> compare x y)

frequencies :: Eq a => [a] -> [(a, Int)]
frequencies list = map (\c -> (c, length . (`elemIndices` list) $ c)) list

mostFrequent :: Eq a => [a] -> a
mostFrequent = fst . tupMax . frequencies

mostPopularLetter :: String -> Char
mostPopularLetter = mostFrequent . map toLower

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostFrequent . words . map toLower . unwords
