{-# LANGUAGE InstanceSigs#-}
{-# LANGUAGE TupleSections #-}
module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import RandomExample

rollDie :: State StdGen Die
rollDie =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, [])
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum (count, dies) gen
      | sum >= n = (count, dies)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1, intToDie die : dies) nextGen



newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s ->
    let
      (x, st) = g s
    in
      (f x, st)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (a,)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let
      (h, s1) = f s
      (x, s2) = g s1
    in
      (h x, s2)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let
      (x, st) = f s
    in
      runMoi (g x) st

myGet :: Moi s s
myGet = Moi $ \s -> (s, s)

myPut :: s -> Moi s ()
myPut s = Moi $ const ((), s)

myExec :: Moi s a -> s -> s
myExec (Moi sa) = snd . sa

myEval :: Moi s a -> s -> a
myEval (Moi sa) = fst . sa

myModify :: (s -> s) -> Moi s ()
myModify f = Moi $ \x -> ((), f x)
