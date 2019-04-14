{-# LANGUAGE DeriveFunctor #-}

module Control.Monad.TrackedIO where

import           Control.Applicative     (Alternative (..))
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift

data TrackedIO a
  = NoIOUsed a
  | TrackedIO (IO a)
  deriving (Functor)

instance Applicative TrackedIO where
  pure = NoIOUsed
  NoIOUsed f <*> NoIOUsed a = NoIOUsed (f a)
  f <*> a = TrackedIO (getTrackedIO f <*> getTrackedIO a)

instance Monad TrackedIO where
  NoIOUsed a >>= f = f a
  TrackedIO a >>= f = TrackedIO (a >>= getTrackedIO . f)

instance MonadIO TrackedIO where
  liftIO = TrackedIO

instance MonadUnliftIO TrackedIO where
  askUnliftIO = NoIOUsed (UnliftIO getTrackedIO)

getTrackedIO :: TrackedIO a -> IO a
getTrackedIO (TrackedIO x) = x
getTrackedIO (NoIOUsed x)  = pure x

getTrackedIOPure :: Alternative f => TrackedIO a -> f a
getTrackedIOPure (TrackedIO _) = empty
getTrackedIOPure (NoIOUsed x)  = pure x
