module Sound.SC3.UGen.FFT.Base where

import Sound.SC3.UGen.Rate (Rate(KR))
import Sound.SC3.UGen.UGen (UGen, mkOsc)

-- | Randomize order of bins.
pv_BinScramble :: UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble buf wp width trg = mkOsc KR "PV_BinScramble" [buf,wp,width,trg] 1 0

-- | Randomly clear bins.
pv_RandComb :: UGen -> UGen -> UGen -> UGen
pv_RandComb buf wp trg = mkOsc KR "PV_RandComb" [buf,wp,trg] 1 0

-- | Cross fade, copying bins in random order.
pv_RandWipe :: UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe ba bb wp trg = mkOsc KR "PV_RandWipe" [ba,bb,wp,trg] 1 0
