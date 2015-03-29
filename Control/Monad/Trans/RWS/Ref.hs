{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | An implementation of @RWST@ built on top of mutable references,
-- providing a proper monad morphism.
--
-- An additional advantage of this transformer over the standard @RWST@
-- transformers in the transformers package is that it does not have space
-- leaks in the writer component. For more information, see
-- <https://mail.haskell.org/pipermail/libraries/2012-October/018599.html>.
module Control.Monad.Trans.RWS.Ref
    ( RWSRefT
    , runRWSRefT
    , runRWSIORefT
    , runRWSSTRefT
    , module Control.Monad.RWS.Class
    ) where

import           Control.Applicative          (Applicative (..))
import           Control.Monad                (ap, liftM)
import           Control.Monad.Catch          (MonadCatch (..), MonadMask (..),
                                               MonadThrow (..))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.RWS.Class
import           Control.Monad.Trans.Control  (defaultLiftBaseWith,
                                               defaultRestoreM)
import           Control.Monad.Trans.Morphism
import           Data.Monoid                  (Monoid, mappend, mempty)
import           Data.Mutable                 (IORef, MCState, MutableRef,
                                               PrimMonad, PrimState, RealWorld,
                                               RefElement, STRef, modifyRef',
                                               newRef, readRef, writeRef)

-- |
--
-- Since 0.1.0
newtype RWSRefT refw refs r w s m a = RWSRefT
    { unRWSRefT :: r -> refw w -> refs s -> m a
    }
    deriving Functor

-- |
--
-- Since 0.1.0
runRWSRefT
    :: ( Monad m
       , w ~ RefElement (refw w)
       , s ~ RefElement (refs s)
       , MCState (refw w) ~ PrimState b
       , MCState (refs s) ~ PrimState b
       , MonadBase b m
       , MutableRef (refw w)
       , MutableRef (refs s)
       , PrimMonad b
       , Monoid w
       )
    => RWSRefT refw refs r w s m a
    -> r
    -> s
    -> m (a, s, w)
runRWSRefT (RWSRefT f) r s0 = do
    (refw, refs) <- liftBase $ (,) `liftM` newRef mempty `ap` newRef s0
    a <- f r refw refs
    (w, s) <- liftBase $ (,) `liftM` readRef refw `ap` readRef refs
    return (a, s, w)
{-# INLINEABLE runRWSRefT #-}

-- |
--
-- Since 0.1.0
runRWSIORefT
    :: ( Monad m
       , RealWorld ~ PrimState b
       , MonadBase b m
       , PrimMonad b
       , Monoid w
       )
    => RWSRefT IORef IORef r w s m a
    -> r
    -> s
    -> m (a, s, w)
runRWSIORefT = runRWSRefT
{-# INLINE runRWSIORefT #-}

-- |
--
-- Since 0.1.0
runRWSSTRefT
    :: ( Monad m
       , ps ~ PrimState b
       , MonadBase b m
       , PrimMonad b
       , Monoid w
       )
    => RWSRefT (STRef ps) (STRef ps) r w s m a
    -> r
    -> s
    -> m (a, s, w)
runRWSSTRefT = runRWSRefT
{-# INLINE runRWSSTRefT #-}

instance Applicative m => Applicative (RWSRefT refw refs r w s m) where
    pure m = RWSRefT $ \_ _ _ -> pure m
    {-# INLINE pure #-}
    RWSRefT f <*> RWSRefT g = RWSRefT $ \x y z -> f x y z <*> g x y z
    {-# INLINE (<*>) #-}
instance Monad m => Monad (RWSRefT refw refs r w s m) where
    return m = RWSRefT $ \_ _ _ -> return m
    {-# INLINE return #-}
    RWSRefT f >>= g = RWSRefT $ \x y z -> do
        a <- f x y z
        unRWSRefT (g a) x y z
    {-# INLINE (>>=) #-}

instance Monad m => MonadReader r (RWSRefT refw refs r w s m) where
    ask = RWSRefT $ \r _ _ -> return r
    {-# INLINE ask #-}
    local f (RWSRefT g) = RWSRefT $ \r w s -> g (f r) w s
instance ( MCState (refw w) ~ PrimState b
         , Monad m
         , w ~ RefElement (refw w)
         , MutableRef (refw w)
         , PrimMonad b
         , MonadBase b m
         , Monoid w
         )
  => MonadWriter w (RWSRefT refw refs r w s m) where
    writer (a, w) = RWSRefT $ \_ ref _ ->
        liftBase $ modifyRef' ref (`mappend` w) >> return a
    {-# INLINE writer #-}
    tell w = RWSRefT $ \_ ref _ -> liftBase $ modifyRef' ref (`mappend` w)
    {-# INLINE tell #-}
    listen (RWSRefT f) = RWSRefT $ \r _ s -> do
        ref <- liftBase (newRef mempty)
        a <- f r ref s
        w <- liftBase (readRef ref)
        return (a, w)
    {-# INLINEABLE listen #-}
    pass (RWSRefT f) = RWSRefT $ \r ref s -> do
        (a, g) <- f r ref s
        liftBase $ modifyRef' ref g
        return a
    {-# INLINEABLE pass #-}
instance ( MCState (refs s) ~ PrimState b
         , Monad m
         , s ~ RefElement (refs s)
         , MutableRef (refs s)
         , PrimMonad b
         , MonadBase b m
         )
  => MonadState s (RWSRefT refw refs r w s m) where
    get = RWSRefT $ \_ _ -> liftBase . readRef
    {-# INLINE get #-}
    put x = seq x $ RWSRefT $ \_ _ -> liftBase . (`writeRef` x)
    {-# INLINE put #-}
instance ( MCState (refw w) ~ PrimState b
         , MCState (refs s) ~ PrimState b
         , Monad m
         , w ~ RefElement (refw w)
         , s ~ RefElement (refs s)
         , MutableRef (refw w)
         , MutableRef (refs s)
         , PrimMonad b
         , MonadBase b m
         , Monoid w
         )
  => MonadRWS r w s (RWSRefT refw refs r w s m)

instance MonadTrans (RWSRefT refw refs r w s) where
    lift f = RWSRefT $ \_ _ _ -> f
    {-# INLINE lift #-}
instance MonadIO m => MonadIO (RWSRefT refw refs r w s m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}
instance MonadBase b m => MonadBase b (RWSRefT refw refs r w s m) where
    liftBase = lift . liftBase
    {-# INLINE liftBase #-}

instance MonadTransControl (RWSRefT refw refs r w s) where
    type StT (RWSRefT refw refs r w s) a = a
    liftWith f = RWSRefT $ \r w s -> f $ \t -> unRWSRefT t r w s
    restoreT f = RWSRefT $ \_ _ _ -> f
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}
instance MonadTransMorphism (RWSRefT refw refs r w s) where
    askUnlift = RWSRefT $ \r w s -> return $ Unlift (\m -> unRWSRefT m r w s)
    {-# INLINE askUnlift #-}

instance MonadBaseControl b m => MonadBaseControl b (RWSRefT refw refs r w s m) where
    type StM (RWSRefT refw refs r w s m) a = StM m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}
instance MonadBaseMorphism b m => MonadBaseMorphism b (RWSRefT refw refs r w s m)

instance MonadThrow m => MonadThrow (RWSRefT refw refs r w s m) where
    throwM = lift . throwM
    {-# INLINE throwM #-}
instance MonadCatch m => MonadCatch (RWSRefT refw refs r w s m) where
    catch (RWSRefT f) g = RWSRefT $ \e w s -> catch (f e w s)
        ((\m -> unRWSRefT m e w s) . g)

instance MonadMask m => MonadMask (RWSRefT refw refs r w s m) where
  mask a = RWSRefT $ \e w s -> mask $ \u -> unRWSRefT (a $ q u) e w s
    where q :: (m a -> m a) -> RWSRefT refw refs r w s m a -> RWSRefT refw refs r w s m a
          q u (RWSRefT b) = RWSRefT (\r w s -> u (b r w s))
  {-# INLINE mask #-}
  uninterruptibleMask a =
    RWSRefT $ \e w s -> uninterruptibleMask $ \u -> unRWSRefT (a $ q u) e w s
      where q :: (m a -> m a) -> RWSRefT refw refs r w s m a -> RWSRefT refw refs r w s m a
            q u (RWSRefT b) = RWSRefT (\r w s -> u (b r w s))
  {-# INLINE uninterruptibleMask #-}
