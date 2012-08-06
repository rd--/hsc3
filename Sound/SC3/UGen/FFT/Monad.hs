-- | Monad constructors for non-deterministic FFT 'UGen's.
module Sound.SC3.UGen.FFT.Monad where

import Sound.SC3.UGen.FFT.ID as F
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen.Lift
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
