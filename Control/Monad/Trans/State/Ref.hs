{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Control.Monad.Trans.State.Ref
    ( StateRefT
    , runStateRefT
    , runStateIORefT
    , runStateSTRefT
    , module Control.Monad.State.Class
    ) where

import Control.Monad.Trans.Morphism
import Control.Monad.Trans.Control
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad
import Control.Applicative
import Data.Mutable
import Control.Monad.Catch

newtype StateRefT ref s m a = StateRefT
    { unStateRefT :: ref s -> m a
    }
    deriving Functor

runStateRefT
    :: ( Monad m
       , s ~ RefElement (ref s)
       , MCState (ref s) ~ PrimState b
       , MonadBase b m
       , MutableRef (ref s)
       , PrimMonad b
       )
    => StateRefT ref s m a
    -> s
    -> m (a, s)
runStateRefT (StateRefT f) v0 = do
    ref <- liftBase $ newRef v0
    a <- f ref
    v <- liftBase $ readRef ref
    return (a, v)

runStateIORefT
    :: ( Monad m
       , RealWorld ~ PrimState b
       , MonadBase b m
       , PrimMonad b
       )
    => StateRefT IORef s m a
    -> s
    -> m (a, s)
runStateIORefT = runStateRefT
{-# INLINE runStateIORefT #-}

runStateSTRefT
    :: ( Monad m
       , ps ~ PrimState b
       , MonadBase b m
       , PrimMonad b
       )
    => StateRefT (STRef ps) s m a
    -> s
    -> m (a, s)
runStateSTRefT = runStateRefT
{-# INLINE runStateSTRefT #-}

instance Applicative m => Applicative (StateRefT ref s m) where
    pure = StateRefT . const . pure
    {-# INLINE pure #-}
    StateRefT f <*> StateRefT g = StateRefT $ \x -> f x <*> g x
    {-# INLINE (<*>) #-}
instance Monad m => Monad (StateRefT ref s m) where
    return = StateRefT . const . return
    {-# INLINE return #-}
    StateRefT f >>= g = StateRefT $ \x -> do
        a <- f x
        unStateRefT (g a) x
    {-# INLINE (>>=) #-}
instance ( MCState (ref s) ~ PrimState b
         , Monad m
         , s ~ RefElement (ref s)
         , MutableRef (ref s)
         , PrimMonad b
         , MonadBase b m
         )
  => MonadState s (StateRefT ref s m) where
    get = StateRefT $ liftBase . readRef
    {-# INLINE get #-}
    put x = StateRefT $ liftBase . (`writeRef` x)
    {-# INLINE put #-}

instance MonadTrans (StateRefT ref s) where
    lift = StateRefT . const
    {-# INLINE lift #-}
instance MonadIO m => MonadIO (StateRefT ref s m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}
instance MonadBase b m => MonadBase b (StateRefT ref s m) where
    liftBase = lift . liftBase
    {-# INLINE liftBase #-}

instance MonadTransControl (StateRefT ref s) where
    type StT (StateRefT ref s) a = a
    liftWith f = StateRefT $ \r -> f $ \t -> unStateRefT t r
    restoreT = StateRefT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}
instance MonadTransMorphism (StateRefT ref s) where
    askRun = StateRefT $ return . flip unStateRefT
    {-# INLINE askRun #-}

instance MonadBaseControl b m => MonadBaseControl b (StateRefT ref s m) where
    type StM (StateRefT ref s m) a = StM m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}
instance MonadBaseMorphism b m => MonadBaseMorphism b (StateRefT ref s m)

instance MonadThrow m => MonadThrow (StateRefT ref s m) where
    throwM = lift . throwM
    {-# INLINE throwM #-}
instance MonadCatch m => MonadCatch (StateRefT ref s m) where
    catch (StateRefT f) g = StateRefT $ \e -> catch (f e) ((`unStateRefT` e) . g)

instance MonadMask m => MonadMask (StateRefT ref s m) where
  mask a = StateRefT $ \e -> mask $ \u -> unStateRefT (a $ q u) e
    where q :: (m a -> m a) -> StateRefT ref s m a -> StateRefT ref s m a
          q u (StateRefT b) = StateRefT (u . b)
  {-# INLINE mask #-}
  uninterruptibleMask a =
    StateRefT $ \e -> uninterruptibleMask $ \u -> unStateRefT (a $ q u) e
      where q :: (m a -> m a) -> StateRefT ref s m a -> StateRefT ref s m a
            q u (StateRefT b) = StateRefT (u . b)
  {-# INLINE uninterruptibleMask #-}
