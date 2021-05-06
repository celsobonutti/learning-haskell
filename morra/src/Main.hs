module Main where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Map (Map)

type Game = (Int, Int)

type GameState = StateT Game IO ()

printLine = liftIO . putStrLn

main :: GameState
main = do
  printLine "-- p is Player"
  printLine "-- c is Computer"
  printLine "-- Player is odds, Computer is evens."
  playAI
  
playAI :: GameState
playAI = do
  liftIO . putStr $ "P: "
  player <- liftIO getLine
  liftIO . putStr $ "C :"
  computer <- liftIO getLine
  playAI
  
