module LearnParsers where

import Text.Trifecta
import Control.Applicative ((<|>))

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'


one' = one >> stop

oneTwo :: Parser Char 
oneTwo = char '1' >> char '2' <* eof

oneTwo' = oneTwo >> stop

oneTwoThree :: Parser String
oneTwoThree = string "123"
           <|> string "12"
           <|> string "1"

string' :: String -> Parser String
string' str = go str ""
  where 
    go (x:xs) current = char x >>= (\x' -> go xs $ current ++ [x'])
    go [] result = return result

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "oneTwoThree: "
  print $ parseString oneTwoThree mempty "123"
  
