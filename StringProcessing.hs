module StringProcessing where

import Data.Maybe (fromMaybe)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

replaceThe :: String -> String
replaceThe "" = ""
replaceThe phrase = fromMaybe "a" . notThe $ firstWord ++ " " ++ replaceThe rest
 where
  (firstWord : tail) = words phrase
  rest = unwords tail

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel phrase =
  case wrds of
    (first : second : rest) ->
      let toSum = if first == "the" && (isVowel . head) second then 1 else 0
       in toSum + (countTheBeforeVowel . tail . unwords $ wrds)
    _ -> 0
 where
  wrds = words phrase

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel
