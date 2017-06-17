-- | Envelope / UGen.
module Sound.SC3.UGen.Envelope where

import Sound.SC3.Common.Envelope

import Sound.SC3.UGen.Bindings
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

{- | Trapezoidal envelope generator.

> import Sound.SC3.Plot
> plotEnvelope [envTrapezoid 0.99 0.5 1 1,envTrapezoid 0.5 0.75 0.65 0.35]

-}
envTrapezoid :: OrdE t => t -> t -> t -> t -> Envelope t
envTrapezoid = envTrapezoid_f ((<=*),(>=*))

-- | Singleton fade envelope.
envGate :: UGen -> UGen -> UGen -> DoneAction -> Envelope_Curve UGen -> UGen
envGate level gate_ fadeTime doneAction curve =
    let startVal = fadeTime <=* 0
        e = Envelope [startVal,1,0] [1,1] [curve] (Just 1) Nothing
    in envGen KR gate_ level 0 fadeTime doneAction e

-- | Variant with default values for all inputs.  @gate@ and
-- @fadeTime@ are 'control's, @doneAction@ is 'RemoveSynth', @curve@
-- is 'EnvSin'.
envGate' :: UGen
envGate' =
    let level = 1
        gate_ = meta_control KR "gate" 1 (0,1,"lin",1,"")
        fadeTime = meta_control KR "fadeTime" 0.02 (0,10,"lin",0,"s")
        doneAction = RemoveSynth
        curve = EnvSin
    in envGate level gate_ fadeTime doneAction curve
