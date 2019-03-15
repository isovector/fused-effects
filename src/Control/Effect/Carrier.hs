{-# LANGUAGE DefaultSignatures, DeriveFunctor, FlexibleInstances, FunctionalDependencies, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints                                                                                         #-}

module Control.Effect.Carrier
( HFunctor(..)
, Effect(..)
, Carrier(..)
, handlePure
, handleCoercible
, handleReader
, handleState
, handleEither
, handleTraversable
, interpret
) where

import Control.Monad (join)
import Data.Coerce

class (forall m. Functor m => Functor (h m)) => HFunctor h where
  -- | Functor map. This is required to be 'fmap'.
  --
  --   This can go away once we have quantified constraints.
  fmap' :: Functor m => (a -> b) -> (h m a -> h m b)
  default fmap' :: Functor (h m) => (a -> b) -> (h m a -> h m b)
  fmap' = fmap
  {-# INLINE fmap' #-}

  -- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
  hmap :: (Functor m, Functor n) => (forall x . m x -> n x) -> (h m a -> h n a)


-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended state.
class HFunctor sig => Effect sig where
  -- | Handle any effects in a signature by threading the carrier’s state all the way through to the continuation.
  handle :: (Functor f, Functor n, Functor m)
         => f ()
         -> (forall x . f (m x) -> n (f x))
         -> sig m (m a)
         -> sig n (n (f a))


-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m (m a) -> m a

  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  ret :: a -> m a
  ret = pure

{-# DEPRECATED ret "Use 'pure' instead; 'ret' is a historical alias and will be removed in future versions" #-}

-- | Apply a handler specified as a natural transformation to both higher-order and continuation positions within an 'HFunctor'.
handlePure :: (HFunctor sig, Functor f, Functor g) => (forall x . f x -> g x) -> sig f (f a) -> sig g (g a)
handlePure handler = hmap handler . fmap' handler
{-# INLINE handlePure #-}

-- | Thread a 'Coercible' carrier through an 'HFunctor'.
--
--   This is applicable whenever @f@ is 'Coercible' to @g@, e.g. simple @newtype@s.
handleCoercible :: (HFunctor sig, Coercible f g, Functor f, Functor g) => sig f (f a) -> sig g (g a)
handleCoercible = handlePure coerce
{-# INLINE handleCoercible #-}

{-# DEPRECATED handleReader, handleState, handleEither, handleTraversable
  "Compose carrier types from other carriers and define 'eff' with handleCoercible instead" #-}

-- | Thread a @Reader@-like carrier through an 'HFunctor'.
handleReader :: (HFunctor sig, Functor f, Functor g) => r -> (forall x . f x -> r -> g x) -> sig f (f a) -> sig g (g a)
handleReader r run = handlePure (flip run r)
{-# INLINE handleReader #-}

-- | Thread a @State@-like carrier through an 'Effect'.
handleState :: (Effect sig, Functor f, Functor g) => s -> (forall x . f x -> s -> g (s, x)) -> sig f (f a) -> sig g (g (s, a))
handleState s run = handle (s, ()) (uncurry (flip run))
{-# INLINE handleState #-}

-- | Thread a carrier producing 'Either's through an 'Effect'.
handleEither :: (Carrier sig g, Effect sig, Functor f) => (forall x . f x -> g (Either e x)) -> sig f (f a) -> sig g (g (Either e a))
handleEither run = handle (Right ()) (either (pure . Left) run)
{-# INLINE handleEither #-}

-- | Thread a carrier producing values in a 'Traversable' 'Monad' (e.g. '[]') through an 'Effect'.
handleTraversable :: (Effect sig, Applicative g, Monad m, Traversable m, Functor f) => (forall x . f x -> g (m x)) -> sig f (f a) -> sig g (g (m a))
handleTraversable run = handle (pure ()) (fmap join . traverse run)
{-# INLINE handleTraversable #-}

-- | A backwards-compatibility shim, equivalent to 'id'.
interpret :: carrier a -> carrier a
interpret = id
{-# DEPRECATED interpret "Not necessary with monadic carriers; remove or replace with 'id'." #-}
