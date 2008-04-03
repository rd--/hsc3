module Sound.SC3.UGen.External where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct

ay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen 
   -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
ay ta tb tc n c va vb vc ef es ct = 
    mkOsc AR "AY" [ta, tb, tc, n, c, va, vb, vc, ef, es, ct] 1

ayFreqToTone :: Fractional a => a -> a
ayFreqToTone f = 110300 / (f - 0.5)

membraneCircle :: UGen -> UGen -> UGen -> UGen
membraneCircle i t l = mkOsc AR "MembraneCircle" [i, t, l] 1

membraneHexagon :: UGen -> UGen -> UGen -> UGen
membraneHexagon i t l = mkOsc AR "MembraneHexagon" [i, t, l] 1
