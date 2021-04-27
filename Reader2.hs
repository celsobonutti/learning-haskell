{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  rev'd <- rev
  cap'd <- cap
  return (rev'd, cap'd)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' =
  rev >>= \rev'd -> cap >>= \cap'd -> return (rev'd, cap'd)

ask :: Reader a a
ask = Reader id

newtype HumanName
  = HumanName String
  deriving (Eq, Show)

newtype DogName
  = DogName String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName,
    dogsAddress :: Address
  }

pers :: Person
pers =
  Person
    (HumanName "Big Bird")
    (DogName "Barkley")
    (Address "Sesame Street")

chris :: Person
chris =
  Person
    (HumanName "Chris Allen")
    (DogName "Papu")
    (Address "Austin")

getDog :: Person -> Dog
getDog =
  Dog <$> dogName <*> address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) ::
    Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> (rab r <$> ra) r

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r) r

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy
