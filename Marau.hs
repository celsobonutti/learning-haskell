module Marau where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

parseFirstLine :: Parser (Int, Int)
parseFirstLine =
  (,)
    <$> decimal
    <* char ' '
    <*> decimal

parseSecondLine :: Int -> Parser [Int]
parseSecondLine n = do
  x <- decimal `sepBy` space
  if length x == n
    then return x
    else fail "wrong number of inputs"

main = do
  firstLine <- getLine
  case parse parseFirstLine mempty firstLine of
    Left err ->
      putStrLn $ errorBundlePretty err
    Right (n, limit) -> do
      secondLine <- getLine
      case parse (parseSecondLine n) mempty secondLine of
        Left err ->
          putStrLn $ errorBundlePretty err
        Right rocks ->
          putStrLn $
            if any (> limit) rocks
              then "deu ruim"
              else "deu bom"
