module Control.Effect
( module X
, Eff
) where

import Control.Effect.Carrier   as X (Carrier, Effect)
import Control.Effect.Cull      as X (Cull, CullC, OnceC)
import Control.Effect.Cut       as X (Cut, CutC)
import Control.Effect.Error     as X (Error, ErrorC)
import Control.Effect.Fail      as X (Fail, FailC)
import Control.Effect.Fresh     as X (Fresh, FreshC)
import Control.Effect.Lift      as X (Lift, LiftC, runM)
import Control.Effect.NonDet    as X (NonDet, NonDetC)
import Control.Effect.Pure      as X (Pure, PureC, run)
import Control.Effect.Random    as X (Random, RandomC)
import Control.Effect.Reader    as X (Reader, ReaderC)
import Control.Effect.Resource  as X (Resource, ResourceC)
import Control.Effect.Resumable as X (Resumable, ResumableC, ResumableWithC)
import Control.Effect.State     as X (State, StateC)
import Control.Effect.Sum       as X ((:+:), Member, send)
import Control.Effect.Trace     as X (Trace, TraceByPrintingC, TraceByIgnoringC, TraceByReturningC)
import Control.Effect.Writer    as X (Writer, WriterC)

type Eff m = m
{-# DEPRECATED Eff "Carriers are now monads; use @m@ instead of @Eff m@." #-}
