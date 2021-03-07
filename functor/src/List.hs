module List where

data List a
  = Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons head tail) = Cons (f head) (f <$> tail)
