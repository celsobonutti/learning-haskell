{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString =
  Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]
  
a = "blah"

b = "123"

c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
  >> (Left <$> integer)
    <|> (Right <$> some letter)
  <* skipMany (oneOf "\n")

main = do
  let p f i =
        parseString (some parseNos) mempty i
  print $ p parseNos eitherOr
