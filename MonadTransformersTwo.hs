module MonadTransformersTwo where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad

rDec :: Num a => Reader a a
rDec = reader (subtract 1)

rShow :: Show a => Reader a String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \s -> print ("Henlo " ++ show s) >> return (s + 1)

rPrintIncAccum :: (Num a, Show a) => StateT a IO String
rPrintIncAccum = StateT $ \s -> print ("Henlo " ++ show s) >> return (show s, s + 1)

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn ("Good, was very excite: " ++ e)
