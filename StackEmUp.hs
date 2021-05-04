module StackEmUp where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap = runMaybeT embedded

eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap = runReaderT eitherUnwrap

reEmbedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
reEmbedded =
 MaybeT . ExceptT . ReaderT $ (const . return) (Right (Just 1))
