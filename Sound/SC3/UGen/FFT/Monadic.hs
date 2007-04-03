module Sound.SC3.UGen.FFT.Monadic where

import Sound.SC3.UGen.UGen (UGen, uniquify)
import qualified Sound.SC3.UGen.FFT.Base as F

-- | Randomize order of bins.
pv_BinScramble :: UGen -> UGen -> UGen -> UGen -> IO UGen
pv_BinScramble buf wp width trg = uniquify (F.pv_BinScramble buf wp width trg)

-- | Randomly clear bins.
pv_RandComb :: UGen -> UGen -> UGen -> IO UGen
pv_RandComb buf wp trg = uniquify (F.pv_RandComb buf wp trg)

-- | Cross fade, copying bins in random order.
pv_RandWipe :: UGen -> UGen -> UGen -> UGen -> IO UGen
pv_RandWipe ba bb wp trg = uniquify (F.pv_RandWipe ba bb wp trg)
