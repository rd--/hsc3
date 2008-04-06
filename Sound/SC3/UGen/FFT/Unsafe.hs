module Sound.SC3.UGen.FFT.Unsafe where

import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Lift
import qualified Sound.SC3.UGen.FFT.Monadic as F

-- | Randomize order of bins.
pv_BinScramble :: UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble = liftP4 F.pv_BinScramble

-- | Randomly clear bins.
pv_RandComb :: UGen -> UGen -> UGen -> UGen
pv_RandComb = liftP3 F.pv_RandComb

-- | Cross fade, copying bins in random order.
pv_RandWipe :: UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe = liftP4 F.pv_RandWipe
