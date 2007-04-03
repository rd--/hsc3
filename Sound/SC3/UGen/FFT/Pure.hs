module Sound.SC3.UGen.FFT.Pure where

import Sound.SC3.UGen.UGen (UGen, withUId)
import Sound.SC3.UGen.UId  (UId)
import qualified Sound.SC3.UGen.FFT.Base as F

-- | Randomize order of bins.
pv_BinScramble :: UId -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble d buf wp width trg = withUId d (F.pv_BinScramble buf wp width trg)

-- | Randomly clear bins.
pv_RandComb :: UId -> UGen -> UGen -> UGen -> UGen
pv_RandComb d buf wp trg = withUId d (F.pv_RandComb buf wp trg)

-- | Cross fade, copying bins in random order.
pv_RandWipe :: UId -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe d ba bb wp trg = withUId d (F.pv_RandWipe ba bb wp trg)
