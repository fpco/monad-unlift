{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Morphism
    ( MonadTransMorphism (..)
    , MonadBaseMorphism (..)
    ) where

import Control.Monad (liftM, ap)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Logger (LoggingT (..), NoLoggingT (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Yesod.Core.Types (HandlerT (..))

class MonadTransControl t => MonadTransMorphism t where
    askRun :: Monad m => t m (t m a -> m a)

instance MonadTransMorphism IdentityT where
    askRun = return runIdentityT
    {-# INLINE askRun #-}
instance MonadTransMorphism (ReaderT env) where
    askRun = ReaderT $ return . flip runReaderT
    {-# INLINE askRun #-}
instance MonadTransMorphism LoggingT where
    askRun = LoggingT $ return . flip runLoggingT
    {-# INLINE askRun #-}
instance MonadTransMorphism NoLoggingT where
    askRun = return runNoLoggingT
    {-# INLINE askRun #-}

class MonadBaseControl b m => MonadBaseMorphism b m | m -> b where
    askRunBase :: m (m a -> b a)
    default askRunBase
        :: (MonadBaseMorphism b m, MonadTransMorphism t, Monad (t m))
        => t m (t m a -> b a)
    askRunBase = (.) `liftM` lift askRunBase `ap` askRun
    {-# INLINE askRunBase #-}

instance MonadBaseMorphism b m => MonadBaseMorphism b (IdentityT m)
instance MonadBaseMorphism b m => MonadBaseMorphism b (ReaderT env m)
instance MonadBaseMorphism b m => MonadBaseMorphism b (LoggingT m)
instance MonadBaseMorphism b m => MonadBaseMorphism b (NoLoggingT m)

instance MonadBaseMorphism b m => MonadBaseMorphism b (HandlerT site m) where
    askRunBase = HandlerT $ \env ->
        (. (`unHandlerT` env)) `liftM` askRunBase
    {-# INLINE askRunBase #-}
