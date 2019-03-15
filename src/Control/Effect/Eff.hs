{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Control.Effect.Eff where

import qualified TRYAGAIN as E
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad (join)


------------------------------------------------------------------------------
-- | Generic machinery for lifting discount-monads into fused-effects.
data LiftE e (m :: * -> *) k = LiftE
  { getLiftE :: e m k
  }

deriving instance (Functor (e m)) => Functor (LiftE e m)

instance E.Effect e => HFunctor (LiftE e) where
  fmap' f (LiftE e) = LiftE $ fmap f e
  hmap f (LiftE e) = LiftE $ E.hoist f e

instance E.Effect e => Effect (LiftE e) where
  handle s f (LiftE e) = LiftE $ fmap f $ E.weave s f e


------------------------------------------------------------------------------
-- | Per-interpretation boilerplate :(
newtype EStateC s m a = EStateC
  { runEStateC :: E.Eff '[E.State s, E.Lift m] a
  }
  deriving (Functor, Applicative, Monad)

instance (Carrier sig m, Effect sig) => Carrier (LiftE (E.State s) :+: sig) (EStateC s m) where
  eff (L u) = EStateC
            . join
            . E.send
            . E.hoist runEStateC
            $ fmap runEStateC
            $ getLiftE u
  eff (R other) = EStateC $ do
    s <- E.get @s
    (s', a) <-
      E.sendM . eff
              . handle
                 (s, ())
                 (\(s0, m) -> E.runM $ E.runState s0 $ runEStateC m)
              $  other
    E.put s'
    pure a
  {-# INLINE eff #-}

