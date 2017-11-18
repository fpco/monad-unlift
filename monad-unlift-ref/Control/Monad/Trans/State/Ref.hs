{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | An implementation of @StateT@ built on top of mutable references,
-- providing a proper monad morphism.
--
-- Please see the documentation at
-- <https://www.stackage.org/package/monad-unlift> for more details on using
-- this module.
module Control.Monad.Trans.State.Ref
    ( StateRefT
    , runStateRefT
    , runStateIORefT
    , runStateSTRefT
    , module Control.Monad.State.Class
    ) where

import           Control.Applicative         (Applicative (..))
import           Control.Monad.Catch         (MonadCatch (..), MonadMask (..),
                                              MonadThrow (..))
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Reader.Class  (MonadReader (..))
import           Control.Monad.State.Class
import           Control.Monad.Trans.Control (defaultLiftBaseWith,
                                              defaultRestoreM)
import           Control.Monad.Trans.Unlift
import           Control.Monad.Trans.Resource (MonadResource (..))
import           Data.Mutable                (IORef, MCState, MutableRef,
                                              PrimMonad, PrimState, RealWorld,
                                              RefElement, STRef, newRef,
                                              readRef, writeRef)

-- |
--
-- Since 0.1.0
newtype StateRefT ref s m a = StateRefT
    { unStateRefT :: ref s -> m a
    }
    deriving Functor

-- |
--
-- Since 0.1.0
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
{-# INLINEABLE runStateRefT #-}

-- |
--
-- Since 0.1.0
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

-- |
--
-- Since 0.1.0
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
    put x = seq x $ StateRefT $ liftBase . (`writeRef` x)
    {-# INLINE put #-}

instance MonadReader r m => MonadReader r (StateRefT ref s m) where
    ask = StateRefT $ const ask
    {-# INLINE ask #-}
    local f m = StateRefT $ local f . unStateRefT m
    {-# INLINE local #-}
    reader = StateRefT . const . reader
    {-# INLINE reader #-}

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

instance MonadBaseControl b m => MonadBaseControl b (StateRefT ref s m) where
    type StM (StateRefT ref s m) a = StM m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

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

instance MonadResource m => MonadResource (StateRefT ref s m) where
    liftResourceT = lift . liftResourceT
    {-# INLINE liftResourceT #-}
