-- | Bindings to unit generators in sc3-plugins.
module Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins where

import Sound.SC3.Common.Rate
import qualified Sound.SC3.UGen.Bindings.HW.Construct as C
import Sound.SC3.UGen.Type

-- | Convert frequency value to value appropriate for AY tone inputs.
ayFreqToTone :: Fractional a => a -> a
ayFreqToTone f = 110300 / (f - 0.5)

-- | LADSPA plugins inside SuperCollider.
ladspa :: Int -> Rate -> UGen -> [UGen] -> UGen
ladspa nc rt k z = C.mkOsc rt "LADSPA" (constant nc : k : z) nc
