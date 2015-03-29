{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Morphism
    ( MonadTransMorphism (..)
    , Unlift (..)
    , askRun
    , UnliftBase (..)
    , askRunBase
    , MonadBaseMorphism (..)
    ) where

import Control.Monad.STM (STM)
import Control.Monad.ST (ST)
import Data.Functor.Identity (Identity)
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Logger (LoggingT (..), NoLoggingT (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Yesod.Core.Types (HandlerT (..))
import Control.Monad.Trans.Resource.Internal (ResourceT (..))

newtype Unlift t = Unlift { unlift :: forall a n. Monad n => t n a -> n a }

class MonadTransControl t => MonadTransMorphism t where
    askUnlift :: Monad m => t m (Unlift t)

askRun :: (MonadTransMorphism t, Monad (t m), Monad m) => t m (t m a -> m a)
askRun = liftM unlift askUnlift
{-# INLINE askRun #-}

instance MonadTransMorphism IdentityT where
    askUnlift = return (Unlift runIdentityT)
    {-# INLINE askUnlift #-}
instance MonadTransMorphism (ReaderT env) where
    askUnlift = ReaderT $ \env -> return $ Unlift (`runReaderT` env)
    {-# INLINE askUnlift #-}
instance MonadTransMorphism LoggingT where
    askUnlift = LoggingT $ \f -> return $ Unlift (`runLoggingT` f)
    {-# INLINE askUnlift #-}
instance MonadTransMorphism NoLoggingT where
    askUnlift = return (Unlift runNoLoggingT)
    {-# INLINE askUnlift #-}
instance MonadTransMorphism ResourceT where
    askUnlift = ResourceT $ \is -> return $ Unlift (`unResourceT` is)
    {-# INLINE askUnlift #-}

newtype UnliftBase b m = UnliftBase { unliftBase :: forall a. m a -> b a }

askRunBase :: (MonadBaseMorphism b m)
           => m (m a -> b a)
askRunBase = liftM unliftBase askUnliftBase
{-# INLINE askRunBase #-}

class MonadBaseControl b m => MonadBaseMorphism b m | m -> b where
    askUnliftBase :: m (UnliftBase b m)
    default askUnliftBase
        :: (MonadBaseMorphism b m, MonadTransMorphism t, Monad (t m))
        => t m (UnliftBase b (t m))
    askUnliftBase = do
        UnliftBase f <- lift askUnliftBase
        Unlift g <- askUnlift
        return $ UnliftBase (f . g)
    {-# INLINE askUnliftBase #-}

instance MonadBaseMorphism IO IO where
    askUnliftBase = return (UnliftBase id)
    {-# INLINE askUnliftBase #-}
instance MonadBaseMorphism STM STM where
    askUnliftBase = return (UnliftBase id)
    {-# INLINE askUnliftBase #-}
instance MonadBaseMorphism Identity Identity where
    askUnliftBase = return (UnliftBase id)
    {-# INLINE askUnliftBase #-}
instance MonadBaseMorphism Maybe Maybe where
    askUnliftBase = return (UnliftBase id)
    {-# INLINE askUnliftBase #-}
instance MonadBaseMorphism (Either e) (Either e) where
    askUnliftBase = return (UnliftBase id)
    {-# INLINE askUnliftBase #-}
instance MonadBaseMorphism (ST s) (ST s) where
    askUnliftBase = return (UnliftBase id)
    {-# INLINE askUnliftBase #-}

instance MonadBaseMorphism b m => MonadBaseMorphism b (IdentityT m)
instance MonadBaseMorphism b m => MonadBaseMorphism b (ReaderT env m)
instance MonadBaseMorphism b m => MonadBaseMorphism b (LoggingT m)
instance MonadBaseMorphism b m => MonadBaseMorphism b (NoLoggingT m)
instance MonadBaseMorphism b m => MonadBaseMorphism b (ResourceT m)

instance MonadBaseMorphism b m => MonadBaseMorphism b (HandlerT site m) where
    askUnliftBase = HandlerT $ \env -> do
        UnliftBase f <- askUnliftBase
        return $ UnliftBase (f . (`unHandlerT` env))
    {-# INLINE askUnliftBase #-}
