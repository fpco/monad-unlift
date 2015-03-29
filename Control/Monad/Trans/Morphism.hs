{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- | See overview in the README.md
module Control.Monad.Trans.Morphism
    ( -- * Trans
      MonadTransMorphism (..)
    , Unlift (..)
    , askRun
      -- * Base
    , UnliftBase (..)
    , askRunBase
    , MonadBaseMorphism (..)
      -- * Reexports
    , MonadTrans (..)
    , MonadBase (..)
    , MonadTransControl (..)
    , MonadBaseControl (..)
    ) where

import Control.Monad.STM (STM)
import Control.Monad.ST (ST)
import Data.Functor.Identity (Identity)
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Control (MonadTransControl (..), MonadBaseControl (..))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Logger (LoggingT (..), NoLoggingT (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Yesod.Core.Types (HandlerT (..))
import Control.Monad.Trans.Resource.Internal (ResourceT (..))

-- | A function which can move an action down the monad transformer stack, by
-- providing any necessary environment to the action.
--
-- Note that, if ImpredicativeTypes worked reliably, this type wouldn't be
-- necessary, and 'askUnlift' would simply include a more generalized type.
--
-- Since 0.1.0
newtype Unlift t = Unlift { unlift :: forall a n. Monad n => t n a -> n a }

-- | A monad transformer which behaves the monad morphism laws.
--
-- Since 0.1.0
class MonadTransControl t => MonadTransMorphism t where
    -- | Get the 'Unlift' action for the current transformer layer.
    --
    -- Since 0.1.0
    askUnlift :: Monad m => t m (Unlift t)

-- | A simplified version of 'askUnlift' which addresses the common case where
-- polymorphism isn't necessary.
--
-- Since 0.1.0
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
-- | Note that using this instance is slightly dangerous, in the same way that
-- accessing the @InternalState@ is dangerous in general. Only do so if you
-- know the @runResourceT@ block has not completed.
instance MonadTransMorphism ResourceT where
    askUnlift = ResourceT $ \is -> return $ Unlift (`unResourceT` is)
    {-# INLINE askUnlift #-}

-- | Similar to 'Unlift', but instead of moving one layer down the stack, moves
-- the action to the base monad.
--
-- Since 0.1.0
newtype UnliftBase b m = UnliftBase { unliftBase :: forall a. m a -> b a }

-- | A monad transformer stack which behaves the monad morphism laws.
--
-- Since 0.1.0
class MonadBaseControl b m => MonadBaseMorphism b m | m -> b where
    -- | Get the 'UnliftBase' action for the current transformer stack.
    --
    -- Since 0.1.0
    askUnliftBase :: m (UnliftBase b m)
    default askUnliftBase
        :: (MonadBaseMorphism b m, MonadTransMorphism t, Monad (t m))
        => t m (UnliftBase b (t m))
    askUnliftBase = do
        UnliftBase f <- lift askUnliftBase
        Unlift g <- askUnlift
        return $ UnliftBase (f . g)
    {-# INLINE askUnliftBase #-}

-- | A simplified version of 'askUnliftBase' which addresses the common case
-- where polymorphism isn't necessary.
--
-- Since 0.1.0
askRunBase :: (MonadBaseMorphism b m)
           => m (m a -> b a)
askRunBase = liftM unliftBase askUnliftBase
{-# INLINE askRunBase #-}

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
-- | Same warnings as the 'MonadTransMorphism' class apply.
instance MonadBaseMorphism b m => MonadBaseMorphism b (ResourceT m)

instance MonadBaseMorphism b m => MonadBaseMorphism b (HandlerT site m) where
    askUnliftBase = HandlerT $ \env -> do
        UnliftBase f <- askUnliftBase
        return $ UnliftBase (f . (`unHandlerT` env))
    {-# INLINE askUnliftBase #-}
