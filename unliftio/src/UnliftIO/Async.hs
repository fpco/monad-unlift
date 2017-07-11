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
    asyncThreadId,

    -- ** STM operations
    waitSTM, pollSTM, waitCatchSTM,

    -- ** Waiting for multiple 'Async's
    waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel,
    waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel,
    waitEither_,
    waitBoth,

    -- ** Waiting for multiple 'Async's in STM
    waitAnySTM, waitAnyCatchSTM,
    waitEitherSTM, waitEitherCatchSTM,
    waitEitherSTM_,
    waitBothSTM,

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

import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as A
import Control.Monad.IO.Unlift

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
