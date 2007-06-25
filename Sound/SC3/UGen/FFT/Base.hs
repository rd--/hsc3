module Sound.SC3.UGen.FFT.Base where

import Sound.SC3.UGen.Rate (Rate(KR))
import Sound.SC3.UGen.UGen (UGen, UGenID, mkOsc)

-- | Randomize order of bins.
pv_BinScramble :: UGen -> UGen -> UGen -> UGen -> (UGenID -> UGen)
pv_BinScramble buf wp width trg = mkOsc KR "PV_BinScramble" [buf,wp,width,trg] 1

-- | Randomly clear bins.
pv_RandComb :: UGen -> UGen -> UGen -> (UGenID -> UGen)
pv_RandComb buf wp trg = mkOsc KR "PV_RandComb" [buf,wp,trg] 1

-- | Cross fade, copying bins in random order.
pv_RandWipe :: UGen -> UGen -> UGen -> UGen -> (UGenID -> UGen)
pv_RandWipe ba bb wp trg = mkOsc KR "PV_RandWipe" [ba,bb,wp,trg] 1
