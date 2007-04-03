module Sound.SC3.UGen.FFT.Pure where

import Sound.SC3.UGen.Rate (Rate(KR))
import Sound.SC3.UGen.UGen (UGen, mkOscUId)
import Sound.SC3.UGen.UId  (UId)

-- | Randomize order of bins.
pv_BinScramble :: UId -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble uid buf wp width trg = mkOscUId uid KR "PV_BinScramble" [buf,wp,width,trg] 1 0

-- | Randomly clear bins.
pv_RandComb :: UId -> UGen -> UGen -> UGen -> UGen
pv_RandComb uid buf wp trg = mkOscUId uid KR "PV_RandComb" [buf,wp,trg] 1 0

-- | Cross fade, copying bins in random order.
pv_RandWipe :: UId -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe uid ba bb wp trg = mkOscUId uid KR "PV_RandWipe" [ba,bb,wp,trg] 1 0
