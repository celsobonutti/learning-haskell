module MonadFunctions where

j :: Monad m => m (m a) -> m a
j = (=<<) id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = x >>= return . f

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f mx my = do
  x <- mx
  y <- my
  return (f x y)

a :: Monad m => m a -> m (a -> b) -> m b
a mx mf = mf >>= \f -> mx >>= \x -> return (f x)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (head : tail) f =
  f head
    >>= \h ->
      meh tail f
        >>= \t -> return (h : t)

flipType :: Monad m => [m a] -> m [a]
flipType x = meh x id
