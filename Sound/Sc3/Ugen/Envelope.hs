-- | Envelope / Ugen.
module Sound.Sc3.Ugen.Envelope where

import Sound.Sc3.Common.Enum
import Sound.Sc3.Common.Envelope
import Sound.Sc3.Common.Math.Operator
import Sound.Sc3.Common.Rate

import Sound.Sc3.Ugen.Bindings
import Sound.Sc3.Ugen.Ugen
import Sound.Sc3.Ugen.Util

{- | Trapezoidal envelope generator.

> import Sound.Sc3.Plot
> plotEnvelope [envTrapezoid 0.99 0.5 1 1,envTrapezoid 0.5 0.75 0.65 0.35]

-}
envTrapezoid :: OrdE t => t -> t -> t -> t -> Envelope t
envTrapezoid = envTrapezoid_f (less_than_or_equal_to,greater_than_or_equal_to)

-- | 'latch' 1 of 'impulse' 0.
first_zero_then_one :: Rate -> Ugen
first_zero_then_one rt = latch 1 (impulse rt 0 0)

-- | 'env_circle_z' of k-rate 'first_zero_thereafter_one'.
env_circle_u :: Ugen -> Envelope_Curve Ugen -> Envelope Ugen -> Envelope Ugen
env_circle_u = env_circle_z (first_zero_then_one ControlRate)

-- | Singleton fade envelope.
envGate :: Ugen -> Ugen -> Ugen -> DoneAction Ugen -> Envelope_Curve Ugen -> Ugen
envGate level gate_ fadeTime doneAction curve =
    let startVal = fadeTime `less_than_or_equal_to` 0
        e = Envelope [startVal,1,0] [1,1] [curve] (Just 1) Nothing 0
    in envGen ControlRate gate_ level 0 fadeTime doneAction e

-- | Variant with default values for all inputs.  @gate@ and
-- @fadeTime@ are 'control's, @doneAction@ is 'RemoveSynth', @curve@
-- is 'EnvSin'.
envGate_def :: Ugen
envGate_def =
    let level = 1
        gate_ = control_m ControlRate "gate" 1 (0,1,"gate")
        fadeTime = control_m ControlRate "fadeTime" 0.02 (0,10,"lin")
        doneAction = RemoveSynth
        curve = EnvSin
    in envGate level gate_ fadeTime doneAction curve
