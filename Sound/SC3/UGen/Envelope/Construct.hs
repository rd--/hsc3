-- | Functions to generate break point data for standard envelope
--   types.
module Sound.SC3.UGen.Envelope.Construct where

import Sound.SC3.Common.Envelope

import Sound.SC3.UGen.Bindings
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

{- | Trapezoidal envelope generator.

The arguments are: 1. @shape@ determines the sustain time as a
proportion of @dur@, zero is a triangular envelope, one a rectangular
envelope; 2. @skew@ determines the attack\/decay ratio, zero is an
immediate attack and a slow decay, one a slow attack and an immediate
decay; 3. @duration@ in seconds; 4. @amplitude@ as linear gain.

> plotEnvelope [envTrapezoid 0.99 0.5 1 1]

-}
envTrapezoid :: OrdE a => a -> a -> a -> a -> Envelope a
envTrapezoid shape skew dur amp =
    let x1 = skew * (1 - shape)
        bp = [(0,skew <=* 0)
             ,(x1,1)
             ,(shape + x1,1)
             ,(1,skew >=* 1)]
    in envCoord bp dur amp EnvLin

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
