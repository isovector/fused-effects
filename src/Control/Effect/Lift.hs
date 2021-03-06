{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Lift
( Lift(..)
, sendM
, runM
, LiftC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce

newtype Lift sig (m :: * -> *) k = Lift { unLift :: sig k }
  deriving (Functor)

instance Functor sig => HFunctor (Lift sig) where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Functor sig => Effect (Lift sig) where
  handle state handler (Lift op) = Lift (fmap (handler . (<$ state)) op)


-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
runM :: LiftC m a -> m a
runM = runLiftC

-- | Given a @Lift n@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
sendM :: (Member (Lift n) sig, Carrier sig m, Functor n) => n a -> m a
sendM = send . Lift . fmap pure

newtype LiftC m a = LiftC { runLiftC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadPlus)

instance MonadTrans LiftC where
  lift = LiftC

instance Monad m => Carrier (Lift m) (LiftC m) where
  eff = LiftC . (>>= runLiftC) . unLift
