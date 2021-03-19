module Person where

import Validation

data Error
  = EmptyName
  | NegativeAge
  | NegativeHeight
  deriving (Eq, Show)

instance Semigroup Error where
  x <> _ = x

instance Monoid Error where
  mempty = EmptyName

newtype Name = Name String
  deriving (Eq, Show)

newtype Age = Age Int
  deriving (Eq, Show)

newtype Height = Height Float
  deriving (Eq, Show)

data Person = Person
  { name :: Name,
    age :: Age,
    height :: Height
  }
  deriving (Eq, Show)

validateName :: String -> Validation Error Name
validateName "" = Failure' [EmptyName]
validateName n = Success' $ Name n

validateAge :: Int -> Validation Error Age
validateAge x
  | x < 0 = Failure' [NegativeAge]
  | otherwise = Success' $ Age x

validateHeight :: Float -> Validation Error Height
validateHeight x
  | x < 0 = Failure' [NegativeHeight]
  | otherwise = Success' $ Height x

validatePerson :: String -> Int -> Float -> Validation Error Person
validatePerson n a h =
  (Person <$> validateName n) <*> validateAge a <*> validateHeight h
