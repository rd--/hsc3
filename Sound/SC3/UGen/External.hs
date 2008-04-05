module Sound.SC3.UGen.External where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct

-- | Resynthesize sinusoidal ATS analysis data.
atsSynth :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atsSynth b np ps pk fp m a = mkOsc AR "AtsSynth" [b, np, ps, pk, fp, m, a] 1

-- | Resynthesize sinusoidal and critical noise ATS analysis data.
atsNoiSynth :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atsNoiSynth b np ps pk fp sr nr m a nb bs bk = mkOsc AR "AtsNoiSynth" [b, np, ps, pk, fp, sr, nr, m, a, nb, bs, bk] 1

-- | Emulation of AY (aka YM) soundchip, used in Spectrum/Atari.
ay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
ay ta tb tc n c va vb vc ef es ct = mkOsc AR "AY" [ta, tb, tc, n, c, va, vb, vc, ef, es, ct] 1

-- | Convert frequency value to value appropriate for AY tone inputs.
ayFreqToTone :: Fractional a => a -> a
ayFreqToTone f = 110300 / (f - 0.5)

-- | Triangular waveguide mesh of a drum-like membrane.
membraneCircle :: UGen -> UGen -> UGen -> UGen
membraneCircle i t l = mkOsc AR "MembraneCircle" [i, t, l] 1

-- | Triangular waveguide mesh of a drum-like membrane.
membraneHexagon :: UGen -> UGen -> UGen -> UGen
membraneHexagon i t l = mkOsc AR "MembraneHexagon" [i, t, l] 1

-- | Extract cps, rmso and err signals from LPC data.
lpcVals :: Rate -> UGen -> UGen -> UGen
lpcVals r b ptr = mkOsc r "LPCVals" [b, ptr] 3

-- | Resynthesize LPC analysis data.
lpcSynth :: UGen -> UGen -> UGen -> UGen
lpcSynth b s ptr = mkOsc AR "LPCSynth" [b, s, ptr] 1

-- | Invert FFT amplitude data.
pv_Invert :: UGen -> UGen
pv_Invert b = mkOsc KR "PV_Invert" [b] 1

-- | Vocal simulation due to W. Kaegi.
vosim :: UGen -> UGen -> UGen -> UGen -> UGen
vosim t f nc d = mkOsc AR "VOSIM" [t, f, nc, d] 1

-- Local Variables:
-- truncate-lines:t
-- End:
