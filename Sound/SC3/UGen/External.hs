module Sound.SC3.UGen.External where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct

ay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
ay ta tb tc n c va vb vc ef es ct = mkOsc AR "AY" [ta, tb, tc, n, c, va, vb, vc, ef, es, ct] 1

ayFreqToTone :: Fractional a => a -> a
ayFreqToTone f = 110300 / (f - 0.5)

membraneCircle :: UGen -> UGen -> UGen -> UGen
membraneCircle i t l = mkOsc AR "MembraneCircle" [i, t, l] 1

membraneHexagon :: UGen -> UGen -> UGen -> UGen
membraneHexagon i t l = mkOsc AR "MembraneHexagon" [i, t, l] 1

lpcVals :: Rate -> UGen -> UGen -> UGen
lpcVals r b ptr = mkOsc r "LPCVals" [b, ptr] 3

lpcSynth :: UGen -> UGen -> UGen -> UGen
lpcSynth b s ptr = mkOsc AR "LPCSynth" [b, s, ptr] 1

pv_Invert :: UGen -> UGen
pv_Invert b = mkOsc KR "PV_Invert" [b] 1

-- Local Variables:
-- truncate-lines:t
-- End:
