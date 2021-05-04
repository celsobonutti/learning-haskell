module PhoneParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

type DDD = Int
type FirstPart = Int
type SecondPart = Int

data Phone
  = Phone { ddd :: DDD
          , firstPart :: FirstPart
          , secondPart :: SecondPart
          } deriving (Eq, Show)

dddP :: Parser DDD
dddP =
  between (char '(') (char ')') p
  <|> p
  where
    p :: Parser DDD
    p = do
      _ <- optional $ char '0'
      x <- count 2 digitChar
      return . read $ x

firstPartP :: Parser Int
firstPartP = do
  nine <- optional $ char '9'
  case nine of
    Nothing -> read <$> count 4 digitChar <?> "home phone start"
    Just x -> read . (x:) <$> count 4 digitChar <?> "mobile phone start"

secondPartP :: Parser Int
secondPartP =
  read <$> count 4 digitChar <?> "second phone part"

phoneP :: Parser Phone
phoneP  = Phone
        <$> dddP
        <* optional (char ' ')
        <*> firstPartP
        <* optional (char '-' <|> char ' ')
        <*> secondPartP
