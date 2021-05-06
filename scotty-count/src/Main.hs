{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config { counts :: IORef (M.Map Text Integer)
         , prefix :: Text
         }

type Scotty
  = ScottyT Text (ReaderT Config IO)

type Handler
  = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (newMap, newCount)
  where 
    newMap = M.insertWith (+) k 1 m 
    newCount = newMap M.! k

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    config <- lift ask
    let key' = prefix config <> unprefixed
        ref = counts config
    (newCounts, newInteger) <- liftIO $ bumpBoomp key' <$> readIORef ref
    liftIO $ writeIORef (counts config) newCounts
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"]

main :: IO ()
main = do
  [prefixArgs] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ TL.pack prefixArgs
      runR = (`runReaderT` config)
  scottyT 3000 runR app
