{-# LANGUAGE OverloadedStrings#-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = do
  integral <- integer
  char '.'
  decimal <- integer
  return $ (fromIntegral integral +) . (fromIntegral decimal /) . (10^) . length . show $ decimal

parseDecimal' = parseString parseDecimal mempty

data Number
  = Fraction Rational
  | Decimal Double
  deriving (Eq, Show)
  
parseNumber :: Parser Number
parseNumber =
  try (Fraction <$> parseFraction)
  <|> try (Decimal <$> parseDecimal)

parseNumber' = parseString parseNumber mempty

main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

onlyInt :: Parser Integer
onlyInt = integer <* eof
