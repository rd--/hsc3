module Sound.SC3.UGen.FFT.Pure where

import Sound.SC3.UGen.UGen
import qualified Sound.SC3.UGen.FFT.Base as F

-- | Randomize order of bins.
pv_BinScramble :: UGenID -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble = liftD4 F.pv_BinScramble

-- | Randomly clear bins.
pv_RandComb :: UGenID -> UGen -> UGen -> UGen -> UGen
pv_RandComb = liftD3 F.pv_RandComb

-- | Cross fade, copying bins in random order.
pv_RandWipe :: UGenID -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe = liftD4 F.pv_RandWipe
