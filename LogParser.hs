module LogParser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

newtype Year = Year Int

newtype Month = Month Int

newtype Day = Day Int

data Date = Date
  { year :: Year,
    month :: Month,
    day :: Day
  }

newtype Hour = Hour Int

newtype Minute = Minute Int

data Time = Time Hour Minute

data Info = Info
  { time :: Time,
    message :: String
  }

data Log = Log
  { date :: Date,
    information :: [Info]
  }

commentP :: Parser ()
commentP = do
  _ <- string "--"
  skipMany (anySingleBut '\n')
  skipMany (char '\n')
  return ()
