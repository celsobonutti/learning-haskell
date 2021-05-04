{-# LANGUAGE InstanceSigs #-}

module MonadTransformers where

import Control.Monad

newtype Identity a = Identity {runIdentity :: a}
  deriving (Eq, Show)

newtype IdentityT f a = IdentityT {runIdentityT :: f a}
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (f <$> fa)

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity x) = Identity (f x)

instance (Applicative m) => Applicative (IdentityT m) where
  pure = IdentityT . pure

  (IdentityT f) <*> (IdentityT x) = IdentityT $ f <*> x

instance Monad Identity where
  return = pure

  (Identity x) >>= f = f x

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT mf) >>= f =
    IdentityT $ mf >>= runIdentityT . f

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ (pure . pure) x

  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ ma >>= fun
    where
      fun Nothing = return Nothing
      fun (Just x) = runMaybeT (f x)

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT x) = EitherT $ (fmap . fmap) f x

instance (Applicative m) => Applicative (EitherT e m) where
  pure x = EitherT $ (pure . pure) x

  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance (Monad m) => Monad (EitherT e m) where
  return = pure

  (EitherT ea) >>= f = EitherT $ ea >>= fun
    where
      fun (Right x) = runEitherT . f $ x
      fun (Left x) = return $ Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) =
  EitherT $ swapEither <$> ema
  where
    swapEither (Left x) = Right x
    swapEither (Right x) = Left x

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT famc fbmc (EitherT amb) = amb >>= fun
  where
    fun (Left x) = famc x
    fun (Right x) = fbmc x

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap ab (ReaderT rma) = ReaderT $ (fmap . fmap) ab rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = ReaderT $ (pure . pure) x

  (ReaderT rmab) <*> (ReaderT rma) =
    ReaderT $ (<*>) <$> rmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (ReaderT rma) >>= armb = ReaderT fun
    where
      fun r = rma r >>= (\a -> (runReaderT $ armb a) r)

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT m) = StateT (fmap fun . m)
    where
      fun (a, st) = (f a, st)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)

  (StateT f) <*> (StateT initialSt) = StateT $ \s -> do
      (x, st) <- initialSt s
      (y, newSt) <- f st
      return (y x, newSt)

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (StateT sma) >>= f = StateT $ \s -> do
    (x, st) <- sma s
    runStateT (f x) st
