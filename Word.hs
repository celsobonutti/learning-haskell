module Word where

newtype Word'
  = Word' String
  deriving (Eq, Show)

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = (`elem` vowels)

mkWord :: String -> Maybe Word'
mkWord word =
  if vowelCount > consonantCount
    then Nothing
    else (Just . Word') word
 where
  vowelCount = length . filter isVowel $ word
  consonantCount = length word - vowelCount