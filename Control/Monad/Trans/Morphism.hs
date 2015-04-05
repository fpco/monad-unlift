{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
-- | See overview in the README.md
module Control.Monad.Trans.Morphism
    ( -- * Trans
      MonadTransMorphism
    , Unlift (..)
    , askUnlift
    , askRun
      -- * Base
    , MonadBaseMorphism
    , UnliftBase (..)
    , askUnliftBase
    , askRunBase
      -- * Reexports
    , MonadTrans (..)
    , MonadBase (..)
    , MonadTransControl (..)
    , MonadBaseControl (..)
    ) where

import           Control.Monad               (ap, liftM)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.ST            (ST)
import           Control.Monad.STM           (STM)
import           Control.Monad.Trans.Class   (MonadTrans (..))
import           Control.Monad.Trans.Control (MonadBaseControl (..),
                                              MonadTransControl (..))
import           Control.Monad.Trans.Reader  (ReaderT)
import           Data.Constraint             ((:-), (\\))
import           Data.Constraint.Forall      (Forall, inst)
import           Data.Functor.Identity       (Identity)

-- | A function which can move an action down the monad transformer stack, by
-- providing any necessary environment to the action.
--
-- Note that, if ImpredicativeTypes worked reliably, this type wouldn't be
-- necessary, and 'askUnlift' would simply include a more generalized type.
--
-- Since 0.1.0
newtype Unlift t = Unlift { unlift :: forall a n. Monad n => t n a -> n a }

class    (StT t a ~ a) => Identical t a
instance (StT t a ~ a) => Identical t a

-- | A monad transformer which behaves the monad morphism laws.
--
-- Since 0.1.0
class    (MonadTransControl t, Forall (Identical t)) => MonadTransMorphism t
instance (MonadTransControl t, Forall (Identical t)) => MonadTransMorphism t

mkUnlift :: forall t m a . (Forall (Identical t), Monad m)
         => (forall n b. Monad n => t n b -> n (StT t b)) -> t m a -> m a
mkUnlift r act = r act \\ (inst :: Forall (Identical t) :- Identical t a)

-- | Get the 'Unlift' action for the current transformer layer.
--
-- Since 0.1.0
askUnlift :: forall t m. (MonadTransMorphism t, Monad m) => t m (Unlift t)
askUnlift = liftWith unlifter
  where
    unlifter :: (forall n b. Monad n => t n b -> n (StT t b)) -> m (Unlift t)
    unlifter r = return $ Unlift (mkUnlift r)

-- | A simplified version of 'askUnlift' which addresses the common case where
-- polymorphism isn't necessary.
--
-- Since 0.1.0
askRun :: (MonadTransMorphism t, Monad (t m), Monad m) => t m (t m a -> m a)
askRun = liftM unlift askUnlift
{-# INLINE askRun #-}

-- | Similar to 'Unlift', but instead of moving one layer down the stack, moves
-- the action to the base monad.
--
-- Since 0.1.0
newtype UnliftBase b m = UnliftBase { unliftBase :: forall a. m a -> b a }

class    (StM m a ~ a) => IdenticalBase m a
instance (StM m a ~ a) => IdenticalBase m a

-- | A monad transformer stack which behaves the monad morphism laws.
--
-- Since 0.1.0
class (MonadBaseControl b m, Forall (IdenticalBase m)) => MonadBaseMorphism b m | m -> b
instance (MonadBaseControl b m, Forall (IdenticalBase m)) => MonadBaseMorphism b m

mkUnliftBase :: forall m a b. (Forall (IdenticalBase m), Monad b)
             => (forall c. m c -> b (StM m c)) -> m a -> b a
mkUnliftBase r act = r act \\ (inst :: Forall (IdenticalBase m) :- IdenticalBase m a)

-- | Get the 'UnliftBase' action for the current transformer stack.
--
-- Since 0.1.0
askUnliftBase :: forall b m. (MonadBaseMorphism b m) => m (UnliftBase b m)
askUnliftBase = liftBaseWith unlifter
  where
    unlifter :: (forall c. m c -> b (StM m c)) -> b (UnliftBase b m)
    unlifter r = return $ UnliftBase (mkUnliftBase r)

-- | A simplified version of 'askUnliftBase' which addresses the common case
-- where polymorphism isn't necessary.
--
-- Since 0.1.0
askRunBase :: (MonadBaseMorphism b m)
           => m (m a -> b a)
askRunBase = liftM unliftBase askUnliftBase
{-# INLINE askRunBase #-}
