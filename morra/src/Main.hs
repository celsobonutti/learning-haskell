{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (forever)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.Random
import System.Environment (getArgs)
import System.Console.ANSI (clearScreen)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Map (Map)
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char.Lexer

type Game = (Int, Int)

type GameState = StateT Game IO ()

printLine = liftIO . putStrLn

main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["friend"] -> 
      putStrLn  "-- P1 is odds, P2 is evens."
      >> runStateT playWithFriend (0, 0)
    _ -> 
      putStrLn  "-- P is Player"
      >> putStrLn  "-- C is Computer"
      >> putStrLn  "-- Player is odds, computer is evens."
      >> runStateT playWithAI (0, 0)
  return ()

parseInt :: Parsec Void String Integer
parseInt = decimal

getNumber :: IO Integer
getNumber = do
  input <- getLine
  case parse parseInt "" input of
    Left x ->
      putStr "This is not a number, try again: "
      >> getNumber
    Right n ->
      if n >= 2 && n <= 10 then
        return n
      else
        putStr "Your number should be between 2 and 10, try again: "
        >> getNumber
 

getRandomNumber :: IO Integer
getRandomNumber = getStdRandom $ randomR (2, 10)
  
playWithAI :: GameState
playWithAI = forever $ do
  liftIO . putStr $ "P: "
  player <- liftIO getNumber
  computer <- liftIO getRandomNumber
  liftIO . putStrLn $ "C: " ++ show computer
  if even $ player + computer then
    printLine "- C wins"
    
  else
    printLine "- P wins"
  
playWithFriend :: GameState
playWithFriend = forever $ do
  liftIO . putStr $ "P1: "
  player <- liftIO getNumber
  liftIO clearScreen
  liftIO . putStr $ "P2: "
  computer <- liftIO getNumber
  liftIO clearScreen
  if even $ player + computer then
    printLine "- P2 wins"
  else
    printLine "- P1 wins"
