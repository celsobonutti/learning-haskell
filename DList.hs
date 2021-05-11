{-# LANGUAGE NamedFieldPuns #-}
module DList where

import Criterion.Main

newtype DList a
  = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ const []
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL $ const [x]
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = flip unDL []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ unDL xs . (x:)
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append l1 l2 = DL $ unDL l1 . unDL l2
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
        [ bench "concat list" $
          whnf schlemiel 123456
        , bench "concat dlist" $
          whnf constructDlist 123456
        ]

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        }

push :: a -> Queue a -> Queue a
push x Queue { enqueue, dequeue } =
  case enqueue of
    [] -> Queue [x] dequeue
    xs -> Queue (x:xs) dequeue

pop :: Queue a -> Maybe (a, Queue a)
pop Queue { enqueue, dequeue } =
  case (enqueue, dequeue) of
    ([], []) -> Nothing
    (_, x:xs) -> Just (x, Queue enqueue xs)
    (_:_, []) ->
      let
        (x:newDequeue) = reverse enqueue
      in
        Just (x, Queue [] newDequeue)
