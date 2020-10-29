-- | Common unit generator graphs.
module Sound.SC3.UGen.Bindings.Composite.External where

import Sound.SC3.Common.Math {- hsc3 -}
import Sound.SC3.Common.Rate {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}
import Sound.SC3.UGen.UGen {- hsc3 -}

import qualified Sound.SC3.UGen.Bindings.DB.External as External

-- | FM7 variant where input matrices are not in MCE form.
fm7_mx :: [[UGen]] -> [[UGen]] -> UGen
fm7_mx ctlMatrix modMatrix = External.fm7 AR (mce (concat ctlMatrix)) (mce (concat modMatrix))

{- | greyhole re-orders parameters as well as unpacking the input signal.

in1=0.0 in2=0.0 damping=0.0 delayTime=2.0 diffusion=0.5 feedback=0.9 moddepth=0.1 modfreq=2.0 size=1.0
in              delayTime=2.0 damping=0.0 size=1.0 diffusion=0.7 feedback=0.9 modDepth=0.1 modFreq=2.0
-}
greyhole :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
greyhole i delayTime damping size diffusion feedback modDepth modFreq =
  let (i1,i2) = unmce2 i
  in External.greyholeRaw i1 i2 damping delayTime diffusion feedback modDepth modFreq size

-- | pulse signal as difference of two 'sawDPW' signals.
pulseDPW :: Rate -> UGen -> UGen -> UGen
pulseDPW rt freq width =
  let o1 = External.sawDPW rt freq 0
      o2 = External.sawDPW rt freq (wrap_hs (-1,1) (width+width))
  in o1 - o2
