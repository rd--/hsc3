-- | Common unit generator graphs.
module Sound.SC3.UGen.Bindings.Composite.External where

import Sound.SC3.Common.Math
import Sound.SC3.Common.Rate
import Sound.SC3.UGen.Type

import qualified Sound.SC3.UGen.Bindings.DB.External as External

-- | FM7 variant where input matrices are not in MCE form.
fm7_mx :: [[UGen]] -> [[UGen]] -> UGen
fm7_mx ctlMatrix modMatrix = External.fm7 AR (mce (concat ctlMatrix)) (mce (concat modMatrix))

-- | pulse signal as difference of two 'sawDPW' signals.
pulseDPW :: Rate -> UGen -> UGen -> UGen
pulseDPW rt freq width =
  let o1 = External.sawDPW rt freq 0
      o2 = External.sawDPW rt freq (wrap_hs (-1,1) (width+width))
  in o1 - o2
