module Sound.SC3.UGen.FFT.Monadic where

import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct
import qualified Sound.SC3.UGen.FFT.Base as F
import Sound.SC3.UGen.UId

-- | Randomize order of bins.
pv_BinScramble :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
pv_BinScramble = liftU4 F.pv_BinScramble

-- | Randomly clear bins.
pv_RandComb :: (UId m) => UGen -> UGen -> UGen -> m UGen
pv_RandComb = liftU3 F.pv_RandComb

-- | Cross fade, copying bins in random order.
pv_RandWipe :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
pv_RandWipe = liftU4 F.pv_RandWipe
