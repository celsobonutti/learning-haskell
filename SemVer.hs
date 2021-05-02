{-# LANGUAGE OverloadedStrings #-}

module SemVer where

import Control.Monad.Trans.Reader
import Data.Text (Text, pack)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)
import Text.Megaparsec.Error.Builder

type Parser = Parsec Void Text

data PreReleaseID
  = Numerical Integer
  | Textual Text
  deriving (Eq, Show)

newtype PreRelease
  = PR (Maybe [PreReleaseID])
  deriving (Eq, Show)

instance Ord PreReleaseID where
  compare (Numerical x) (Numerical y) = compare x y
  compare (Textual x) (Numerical y) = GT
  compare (Numerical x) (Textual y) = LT
  compare (Textual x) (Textual y) = compare x y

instance Ord PreRelease where
  (PR Nothing) <= (PR Nothing) = True
  (PR (Just _)) <= (PR Nothing) = True
  (PR Nothing) <= (PR (Just _)) = False
  (PR (Just x)) <= (PR (Just y)) = x <= y

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

compField :: Ord a => (T -> a) -> (T, T) -> Ordering
compField f = uncurry compare . both f

compareTuple :: (T, T) -> (Ordering, Ordering, Ordering, Ordering)
compareTuple = do
  (,,,)
    <$> compField major
    <*> compField minor
    <*> compField patch
    <*> compField preRelease

data T = T
  { major :: Integer,
    minor :: Integer,
    patch :: Integer,
    preRelease :: PreRelease,
    buildMetadata :: Maybe [Text]
  }
  deriving (Eq, Show)

instance Ord T where
  compare x y =
    case compareTuple (x, y) of
      (EQ, EQ, EQ, EQ) -> EQ
      (EQ, EQ, EQ, x) -> x
      (EQ, EQ, x, _) -> x
      (EQ, x, _, _) -> x
      (x, _, _, _) -> x

integer :: Parser Integer
integer = do
  n <- some digitChar
  case n of
    ['0'] -> return 0
    '0' : _ -> fail "leading zeros are not allowed"
    _ -> return . read $ n

idParser :: Parser String 
idParser = some (alphaNumChar <|> char '-')

isNumeric :: Char -> Bool
isNumeric = (`elem` ("0123456789" :: [Char]))

textual :: Parser Text
textual = do
  value <- idParser
  if all isNumeric value then
    fail ""
  else
    return (pack value)
 
preReleaseParser :: Parser [PreReleaseID]
preReleaseParser = (try (Numerical <$> integer) <|> (Textual <$> textual)) `sepBy1` char '.'

parser :: Parser T
parser =
  T <$> integer
    <* char '.'
    <*> integer
    <* char '.'
    <*> integer
    <*> (PR <$> optional (char '-' *> preReleaseParser))
    <*> optional (char '+' *> (pack <$> idParser) `sepBy1` char '.')
  
testCase :: Text
testCase = "1.0.0-alpha 1.0.0-alpha.1 1.0.0-alpha.beta 1.0.0-beta 1.0.0-beta.2 1.0.0-beta.11 1.0.0-rc.1 1.0.0"

allGreater :: Text -> Either (ParseErrorBundle Text Void) Bool
allGreater text =
  snd . foldl biggerThanTheLast (minimalVersion, True) <$> versions
  where
    versions = parse (parser `sepBy1` space) mempty text
    minimalVersion = T {major = 0, minor = 0, patch = 0, preRelease = PR Nothing, buildMetadata = Nothing}
    biggerThanTheLast (previous, acc) version = (version, acc && version > previous)

main = hspec $ do
  describe "Ord is correct" $ do
    it "for the semver.org example" $ do
      allGreater testCase `shouldBe` Right True

  describe "fail on leading zero" $ do
    it "for major" $ do
      parse parser mempty `shouldFailOn` "01.0.0"

    it "for minor" $ do
      parse parser mempty `shouldFailOn` "1.01.0"

    it "for patch" $ do
      parse parser mempty `shouldFailOn` "1.1.01"

    it "for pre-release" $ do
      parse parser mempty `shouldFailOn` "1.1.0-000"
