module Sound.SC3.UGen.FFT.ID where

import Sound.SC3.Identifier
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen

-- | Randomize order of bins.
pv_BinScramble :: ID i => i -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble z buf wp width trg = mkOscId z KR "PV_BinScramble" [buf,wp,width,trg] 1

-- | Randomly clear bins.
pv_RandComb :: ID i => i -> UGen -> UGen -> UGen -> UGen
pv_RandComb z buf wp trg = mkOscId z KR "PV_RandComb" [buf,wp,trg] 1

-- | Cross fade, copying bins in random order.
pv_RandWipe :: ID i => i -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe z ba bb wp trg = mkOscId z KR "PV_RandWipe" [ba,bb,wp,trg] 1

-- Local Variables:
-- truncate-lines:t
-- End:
