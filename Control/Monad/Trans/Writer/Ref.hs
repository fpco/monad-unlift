{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | An implementation of @WriterT@ built on top of mutable references,
-- providing a proper monad morphism.
--
-- An additional advantage of this transformer over the standard @WriterT@
-- transformers in the transformers package is that it does not have space
-- leaks. For more information, see
-- <https://mail.haskell.org/pipermail/libraries/2012-October/018599.html>.
module Control.Monad.Trans.Writer.Ref
    ( WriterRefT
    , runWriterRefT
    , runWriterIORefT
    , runWriterSTRefT
    , module Control.Monad.Writer.Class
    ) where

import           Control.Applicative          (Applicative (..))
import           Control.Monad.Catch          (MonadCatch (..), MonadMask (..),
                                               MonadThrow (..))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.Control  (defaultLiftBaseWith,
                                               defaultRestoreM)
import           Control.Monad.Trans.Morphism
import           Control.Monad.Writer.Class
import           Data.Monoid                  (Monoid, mappend, mempty)
import           Data.Mutable                 (IORef, MCState, MutableRef,
                                               PrimMonad, PrimState, RealWorld,
                                               RefElement, STRef, modifyRef',
                                               newRef, readRef, writeRef)

-- |
--
-- Since 0.1.0
newtype WriterRefT ref w m a = WriterRefT
    { unWriterRefT :: ref w -> m a
    }
    deriving Functor

-- |
--
-- Since 0.1.0
runWriterRefT
    :: ( Monad m
       , w ~ RefElement (ref w)
       , MCState (ref w) ~ PrimState b
       , MonadBase b m
       , MutableRef (ref w)
       , PrimMonad b
       , Monoid w
       )
    => WriterRefT ref w m a
    -> m (a, w)
runWriterRefT (WriterRefT f) = do
    ref <- liftBase $ newRef mempty
    a <- f ref
    v <- liftBase $ readRef ref
    return (a, v)
{-# INLINEABLE runWriterRefT #-}

-- |
--
-- Since 0.1.0
runWriterIORefT
    :: ( Monad m
       , RealWorld ~ PrimState b
       , MonadBase b m
       , PrimMonad b
       , Monoid w
       )
    => WriterRefT IORef w m a
    -> m (a, w)
runWriterIORefT = runWriterRefT
{-# INLINE runWriterIORefT #-}

-- |
--
-- Since 0.1.0
runWriterSTRefT
    :: ( Monad m
       , ps ~ PrimState b
       , MonadBase b m
       , PrimMonad b
       , Monoid w
       )
    => WriterRefT (STRef ps) w m a
    -> m (a, w)
runWriterSTRefT = runWriterRefT
{-# INLINE runWriterSTRefT #-}

instance Applicative m => Applicative (WriterRefT ref w m) where
    pure = WriterRefT . const . pure
    {-# INLINE pure #-}
    WriterRefT f <*> WriterRefT g = WriterRefT $ \x -> f x <*> g x
    {-# INLINE (<*>) #-}
instance Monad m => Monad (WriterRefT ref w m) where
    return = WriterRefT . const . return
    {-# INLINE return #-}
    WriterRefT f >>= g = WriterRefT $ \x -> do
        a <- f x
        unWriterRefT (g a) x
    {-# INLINE (>>=) #-}
instance ( MCState (ref w) ~ PrimState b
         , Monad m
         , w ~ RefElement (ref w)
         , MutableRef (ref w)
         , PrimMonad b
         , MonadBase b m
         , Monoid w
         )
  => MonadWriter w (WriterRefT ref w m) where
    writer (a, w) = WriterRefT $ \ref ->
        liftBase $ modifyRef' ref (`mappend` w) >> return a
    {-# INLINE writer #-}
    tell w = WriterRefT $ \ref -> liftBase $ modifyRef' ref (`mappend` w)
    {-# INLINE tell #-}
    listen (WriterRefT f) = lift $ do
        ref <- liftBase (newRef mempty)
        a <- f ref
        w <- liftBase (readRef ref)
        return (a, w)
    {-# INLINEABLE listen #-}
    pass (WriterRefT f) = WriterRefT $ \ref -> do
        (a, g) <- f ref
        liftBase $ modifyRef' ref g
        return a
    {-# INLINEABLE pass #-}

instance MonadTrans (WriterRefT ref w) where
    lift = WriterRefT . const
    {-# INLINE lift #-}
instance MonadIO m => MonadIO (WriterRefT ref w m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}
instance MonadBase b m => MonadBase b (WriterRefT ref w m) where
    liftBase = lift . liftBase
    {-# INLINE liftBase #-}

instance MonadTransControl (WriterRefT ref w) where
    type StT (WriterRefT ref w) a = a
    liftWith f = WriterRefT $ \r -> f $ \t -> unWriterRefT t r
    restoreT = WriterRefT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (WriterRefT ref w m) where
    type StM (WriterRefT ref w m) a = StM m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

instance MonadThrow m => MonadThrow (WriterRefT ref w m) where
    throwM = lift . throwM
    {-# INLINE throwM #-}
instance MonadCatch m => MonadCatch (WriterRefT ref w m) where
    catch (WriterRefT f) g = WriterRefT $ \e -> catch (f e) ((`unWriterRefT` e) . g)

instance MonadMask m => MonadMask (WriterRefT ref w m) where
  mask a = WriterRefT $ \e -> mask $ \u -> unWriterRefT (a $ q u) e
    where q :: (m a -> m a) -> WriterRefT ref w m a -> WriterRefT ref w m a
          q u (WriterRefT b) = WriterRefT (u . b)
  {-# INLINE mask #-}
  uninterruptibleMask a =
    WriterRefT $ \e -> uninterruptibleMask $ \u -> unWriterRefT (a $ q u) e
      where q :: (m a -> m a) -> WriterRefT ref w m a -> WriterRefT ref w m a
            q u (WriterRefT b) = WriterRefT (u . b)
  {-# INLINE uninterruptibleMask #-}
