module Capitalize where

import Data.Char (toUpper)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x : xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph paragraph = unwords . snd . foldl fn (True, []) $ wrds
  where
    wrds = words paragraph
    fn
      (shouldCapitalize, words)
      currentWord =
        ( last currentWord == '.',
          words
            ++ [(if shouldCapitalize then capitalizeWord else id) currentWord]
        )