{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NewType where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

newtype IntStr = IntStr (Int, String) deriving (Eq, Show, TooMany)

instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x && tooMany y
