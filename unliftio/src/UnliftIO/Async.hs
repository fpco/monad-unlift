{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
-- | See "Control.Concurrent.Async"
--
-- @since 0.1.0.0
module UnliftIO.Async
  (
    -- * Asynchronous actions
    Async,
    -- ** Spawning
    async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask,

    -- ** Spawning with automatic 'cancel'ation
    withAsync, withAsyncBound, withAsyncOn, withAsyncWithUnmask,
    withAsyncOnWithUnmask,

    -- ** Querying 'Async's
    wait, poll, waitCatch, cancel, uninterruptibleCancel, cancelWith,
    A.asyncThreadId,

    -- ** STM operations
    A.waitSTM, A.pollSTM, A.waitCatchSTM,

    -- ** Waiting for multiple 'Async's
    waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel,
    waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel,
    waitEither_,
    waitBoth,

    -- ** Waiting for multiple 'Async's in STM
    A.waitAnySTM, A.waitAnyCatchSTM,
    A.waitEitherSTM, A.waitEitherCatchSTM,
    A.waitEitherSTM_,
    A.waitBothSTM,

    -- ** Linking
    link, link2,

    -- * Convenient utilities
    race, race_,
    concurrently, concurrently_,
    mapConcurrently, forConcurrently,
    mapConcurrently_, forConcurrently_,
    replicateConcurrently, replicateConcurrently_,
    Concurrently(..),
  ) where

import Control.Applicative
import Control.Concurrent.Async (Async)
import Control.Exception (SomeException, Exception)
import qualified Control.Concurrent.Async as A
import Control.Concurrent (threadDelay)
import Control.Monad (forever, liftM)
import Control.Monad.IO.Unlift
import Data.Semigroup

async :: MonadUnliftIO m => m a -> m (Async a)
async m = withRunInIO $ \run -> A.async $ run m

asyncBound :: MonadUnliftIO m => m a -> m (Async a)
asyncBound m = withRunInIO $ \run -> A.asyncBound $ run m

asyncOn :: MonadUnliftIO m => Int -> m a -> m (Async a)
asyncOn i m = withRunInIO $ \run -> A.asyncOn i $ run m

asyncWithUnmask :: MonadUnliftIO m => ((forall b. m b -> m b) -> m a) -> m (Async a)
asyncWithUnmask m =
  withUnliftIO $ \u -> A.asyncWithUnmask $ \unmask -> unliftIO u $ m $ liftIO . unmask . unliftIO u

asyncOnWithUnmask :: MonadUnliftIO m => Int -> ((forall b. m b -> m b) -> m a) -> m (Async a)
asyncOnWithUnmask i m =
  withUnliftIO $ \u -> A.asyncOnWithUnmask i $ \unmask -> unliftIO u $ m $ liftIO . unmask . unliftIO u

withAsync :: MonadUnliftIO m => m a -> (Async a -> m b) -> m b
withAsync a b = withUnliftIO $ \u -> A.withAsync (unliftIO u a) (unliftIO u . b)

withAsyncBound :: MonadUnliftIO m => m a -> (Async a -> m b) -> m b
withAsyncBound a b = withUnliftIO $ \u -> A.withAsyncBound (unliftIO u a) (unliftIO u . b)

withAsyncOn :: MonadUnliftIO m => Int -> m a -> (Async a -> m b) -> m b
withAsyncOn i a b = withUnliftIO $ \u -> A.withAsyncOn i (unliftIO u a) (unliftIO u . b)

withAsyncWithUnmask
  :: MonadUnliftIO m
  => ((forall c. m c -> m c) -> m a)
  -> (Async a -> m b)
  -> m b
withAsyncWithUnmask a b =
  withUnliftIO $ \u -> A.withAsyncWithUnmask
    (\unmask -> unliftIO u $ a $ liftIO . unmask . unliftIO u)
    (unliftIO u . b)

withAsyncOnWithUnmask
  :: MonadUnliftIO m
  => Int
  -> ((forall c. m c -> m c) -> m a)
  -> (Async a -> m b)
  -> m b
withAsyncOnWithUnmask i a b =
  withUnliftIO $ \u -> A.withAsyncOnWithUnmask i
    (\unmask -> unliftIO u $ a $ liftIO . unmask . unliftIO u)
    (unliftIO u . b)

wait :: MonadIO m => Async a -> m a
wait = liftIO . A.wait

poll :: MonadIO m => Async a -> m (Maybe (Either SomeException a))
poll = liftIO . A.poll

waitCatch :: MonadIO m => Async a -> m (Either SomeException a)
waitCatch = liftIO . A.waitCatch

cancel :: MonadIO m => Async a -> m ()
cancel = liftIO . A.cancel

uninterruptibleCancel :: MonadIO m => Async a -> m ()
uninterruptibleCancel = liftIO . A.uninterruptibleCancel

-- FIXME do the async wrapping up trick from safe-exceptions
cancelWith :: (Exception e, MonadIO m) => Async a -> e -> m ()
cancelWith a e = liftIO (A.cancelWith a e)

waitAny :: MonadIO m => [Async a] -> m (Async a, a)
waitAny = liftIO . A.waitAny

waitAnyCatch :: MonadIO m => [Async a] -> m (Async a, Either SomeException a)
waitAnyCatch = liftIO . A.waitAnyCatch

waitAnyCancel :: MonadIO m => [Async a] -> m (Async a, a)
waitAnyCancel = liftIO . A.waitAnyCancel

waitAnyCatchCancel :: MonadIO m => [Async a] -> m (Async a, Either SomeException a)
waitAnyCatchCancel = liftIO . A.waitAnyCatchCancel

waitEither :: MonadIO m => Async a -> Async b -> m (Either a b)
waitEither a b = liftIO (A.waitEither a b)

waitEitherCatch :: MonadIO m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch a b = liftIO (A.waitEitherCatch a b)

waitEitherCancel :: MonadIO m => Async a -> Async b -> m (Either a b)
waitEitherCancel a b = liftIO (A.waitEitherCancel a b)

waitEitherCatchCancel :: MonadIO m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel a b = liftIO (A.waitEitherCatchCancel a b)

waitEither_ :: MonadIO m => Async a -> Async b -> m ()
waitEither_ a b = liftIO (A.waitEither_ a b)

waitBoth :: MonadIO m => Async a -> Async b -> m (a, b)
waitBoth a b = liftIO (A.waitBoth a b)

link :: Async a -> IO ()
link = liftIO . A.link

link2 :: Async a -> Async b -> IO ()
link2 a b = liftIO (A.link2 a b)

race :: MonadUnliftIO m => m a -> m b -> m (Either a b)
race a b = withUnliftIO $ \u -> A.race (unliftIO u a) (unliftIO u b)

race_ :: MonadUnliftIO m => m a -> m b -> m ()
race_ a b = withUnliftIO $ \u -> A.race_ (unliftIO u a) (unliftIO u b)

concurrently :: MonadUnliftIO m => m a -> m b -> m (a, b)
concurrently a b = withUnliftIO $ \u -> A.concurrently (unliftIO u a) (unliftIO u b)

concurrently_ :: MonadUnliftIO m => m a -> m b -> m ()
concurrently_ a b = withUnliftIO $ \u -> A.concurrently_ (unliftIO u a) (unliftIO u b)

mapConcurrently :: MonadUnliftIO m => Traversable t => (a -> m b) -> t a -> m (t b)
mapConcurrently f t = withRunInIO $ \run -> A.mapConcurrently (run . f) t

forConcurrently :: MonadUnliftIO m => Traversable t => t a -> (a -> m b) -> m (t b)
forConcurrently t f = withRunInIO $ \run -> A.forConcurrently t (run . f)

mapConcurrently_ :: MonadUnliftIO m => Foldable f => (a -> m b) -> f a -> m ()
mapConcurrently_ f t = withRunInIO $ \run -> A.mapConcurrently_ (run . f) t

forConcurrently_ :: MonadUnliftIO m => Foldable f => f a -> (a -> m b) -> m ()
forConcurrently_ t f = withRunInIO $ \run -> A.forConcurrently_ t (run . f)

replicateConcurrently :: MonadUnliftIO m => Int -> m a -> m [a]
replicateConcurrently i m = withRunInIO $ \run -> A.replicateConcurrently i (run m)

replicateConcurrently_ :: MonadUnliftIO m => Int -> m a -> m ()
replicateConcurrently_ i m = withRunInIO $ \run -> A.replicateConcurrently_ i (run m)

newtype Concurrently m a = Concurrently
  { runConcurrently :: m a
  }

instance Monad m => Functor (Concurrently m) where
  fmap f (Concurrently a) = Concurrently $ liftM f a

instance MonadUnliftIO m => Applicative (Concurrently m) where
  pure = Concurrently . return
  Concurrently fs <*> Concurrently as =
    Concurrently $ (\(f, a) -> f a) <$> concurrently fs as

instance MonadUnliftIO m => Alternative (Concurrently m) where
  empty = Concurrently $ liftIO (forever (threadDelay maxBound))
  Concurrently as <|> Concurrently bs =
    Concurrently $ either id id <$> race as bs

#if MIN_VERSION_base(4,9,0)
-- | Only defined by @async@ for @base >= 4.9@
--
-- @since 2.1.0
instance (MonadUnliftIO m, Semigroup a) => Semigroup (Concurrently m a) where
  (<>) = liftA2 (<>)

-- | @since 2.1.0
instance (Semigroup a, Monoid a, MonadUnliftIO m) => Monoid (Concurrently m a) where
  mempty = pure mempty
  mappend = (<>)
#else
-- | @since 2.1.0
instance (Monoid a, MonadUnliftIO m) => Monoid (Concurrently m a) where
  mempty = pure mempty
  mappend = liftA2 mappend
#endif
