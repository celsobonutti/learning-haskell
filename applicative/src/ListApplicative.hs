module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' x (Cons head tail) = Cons head $ take' (x -1) tail

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons head tail) = Cons (f head) (f <$> tail)

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons fhead ftail) <*> list = (fhead <$> list) `append` (ftail <*> list)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

generateCons :: Arbitrary a => Gen (List a)
generateCons = do
  a <- arbitrary
  Cons a <$> arbitrary

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency
      [ (1, return Nil),
        (2, generateCons)
      ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

newtype ZipList' a
  = ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 3000 l
      ys' =
        let (ZipList' l) = ys
         in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' (Cons fhead ftail)) <*> (ZipList' (Cons head tail)) = ZipList' (Cons (fhead head) (unboxZipList (ZipList' ftail <*> ZipList' tail)))

unboxZipList :: ZipList' a -> List a
unboxZipList (ZipList' x) = x
