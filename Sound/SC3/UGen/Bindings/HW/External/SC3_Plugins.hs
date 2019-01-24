-- | Bindings to unit generators in sc3-plugins.
module Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins where

import qualified Sound.SC3.UGen.Bindings.DB.External as External

-- * AY

-- | Convert frequency value to value appropriate for AY tone inputs.
ayFreqToTone :: Fractional a => a -> a
ayFreqToTone f = 110300 / (f - 0.5)
