module Sound.SC3.UGen.FFT.Base where

import Sound.SC3.UGen.Rate (Rate(KR))
import Sound.SC3.UGen.UGen (UGen, UGenGen, mkOsc)

-- | Randomize order of bins.
pv_BinScramble :: UGen -> UGen -> UGen -> UGen -> UGenGen
pv_BinScramble buf wp width trg = mkOsc KR "PV_BinScramble" [buf,wp,width,trg] 1

-- | Randomly clear bins.
pv_RandComb :: UGen -> UGen -> UGen -> UGenGen
pv_RandComb buf wp trg = mkOsc KR "PV_RandComb" [buf,wp,trg] 1

-- | Cross fade, copying bins in random order.
pv_RandWipe :: UGen -> UGen -> UGen -> UGen -> UGenGen
pv_RandWipe ba bb wp trg = mkOsc KR "PV_RandWipe" [ba,bb,wp,trg] 1
