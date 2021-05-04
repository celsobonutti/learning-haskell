module IntegerParser where

import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void

type Parser = Parsec Void String

parseDigit :: Parser Char
parseDigit = oneOf "0123456789" <?> "digit"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit <?> "integer"

signedInteger :: Parser Integer
signedInteger = do
  sign <- optional . char $ '-'
  case sign of
    Nothing -> base10Integer
    Just _ -> negate <$> base10Integer
