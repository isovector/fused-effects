{-# LANGUAGE DeriveFunctor, ExplicitForAll, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.State.Strict
( State (..)
, get
, gets
, put
, modify
, modifyLazy
, StateC(..)
, runState
, evalState
, execState
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.State.Internal
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (fail)

-- | Run a 'State' effect starting from the passed value.
--
--   prop> run (runState a (pure b)) == (a, b)
runState :: s -> StateC s m a -> m (s, a)
runState = flip runStateC

-- | Run a 'State' effect, yielding the result value and discarding the final state.
--
--   prop> run (evalState a (pure b)) == b
evalState :: forall s m a . Functor m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s

-- | Run a 'State' effect, yielding the final state and discarding the return value.
--
--   prop> run (execState a (pure b)) == a
execState :: forall s m a . Functor m => s -> StateC s m a -> m s
execState s = fmap fst . runState s


newtype StateC s m a = StateC { runStateC :: s -> m (s, a) }
  deriving (Functor)

instance Monad m => Applicative (StateC s m) where
  pure a = StateC (\ s -> pure (s, a))
  StateC f <*> StateC a = StateC $ \ s -> do
    (s', f') <- f s
    (s'', a') <- a s'
    let fa = f' a'
    fa `seq` pure (s'', fa)

instance (Alternative m, Monad m) => Alternative (StateC s m) where
  empty = StateC (const empty)
  StateC l <|> StateC r = StateC (\ s -> l s <|> r s)

instance Monad m => Monad (StateC s m) where
  StateC m >>= f = StateC $ \ s -> do
    (s', a) <- m s
    let fa = f a
    fa `seq` runState s' fa

instance MonadFail m => MonadFail (StateC s m) where
  fail s = StateC (const (fail s))

instance MonadIO m => MonadIO (StateC s m) where
  liftIO io = StateC (\ s -> (,) s <$> liftIO io)

instance (Alternative m, Monad m) => MonadPlus (StateC s m)

instance MonadTrans (StateC s) where
  lift m = StateC (\ s -> (,) s <$> m)

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (StateC s m) where
  eff (L (Get   k)) = StateC (\ s -> runState s (k s))
  eff (L (Put s k)) = StateC (\ _ -> runState s k)
  eff (R other)     = StateC (\ s -> eff (handle (s, ()) (uncurry runState) other))

-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
