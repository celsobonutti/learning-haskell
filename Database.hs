module Database where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  foldr
    ( \item acc ->
        case item of
          DbDate date -> date : acc
          _ -> acc
    )
    []

filterDbNum :: [DatabaseItem] -> [Integer]
filterDbNum =
  foldr
    ( \item acc ->
        case item of
          DbNumber n -> n : acc
          _ -> acc
    )
    []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent =
  maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb =
  sum . filterDbNum

avgDb :: [DatabaseItem] -> Double
avgDb db = s / l
  where
    num = filterDbNum db
    s = fromIntegral . sum $ num
    l = fromIntegral . length $ num