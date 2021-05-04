{-# LANGUAGE OverloadedStrings#-}

module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m, MonadFail m) => m Rational
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
  let attoP = parseOnly parseFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP alsoBad
  print $ attoP badFraction
      
  let parseFraction' =
        parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

onlyInt :: Parser Integer
onlyInt = integer <* eof
