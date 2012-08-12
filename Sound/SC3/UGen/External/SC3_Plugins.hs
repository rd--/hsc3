-- | Bindings to unit generators in sc3-plugins.
module Sound.SC3.UGen.External.SC3_Plugins where

import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- * AY

-- | Emulation of AY (aka YM) soundchip, used in Spectrum\/Atari.
ay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
ay ta tb tc n c va vb vc ef es ct = mkOsc AR "AY" [ta, tb, tc, n, c, va, vb, vc, ef, es, ct] 1

-- | Convert frequency value to value appropriate for AY tone inputs.
ayFreqToTone :: Fractional a => a -> a
ayFreqToTone f = 110300 / (f - 0.5)

-- * Concat

-- | Concatenative cross-synthesis.
concat' :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
concat' ctl src sz sk sd ml fs zcr lms sc st rs = mkOsc AR "Concat" [ctl,src,sz,sk,sd,ml,fs,zcr,lms,sc,st,rs] 1

-- | Concatenative cross-synthesis (variant).
concat2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
concat2 ctl src sz sk sd ml fs zcr lms sc st rs th = mkOsc AR "Concat2" [ctl,src,sz,sk,sd,ml,fs,zcr,lms,sc,st,rs,th] 1

-- * Distortion

-- | Brown noise.
disintegrator :: ID a => a -> UGen -> UGen -> UGen -> UGen
disintegrator z i p m = mkFilterId z "Disintegrator" [i,p,m] 1

-- * Josh

-- | Resynthesize sinusoidal ATS analysis data.
atsSynth :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atsSynth b np ps pk fp m a = mkOsc AR "AtsSynth" [b, np, ps, pk, fp, m, a] 1

-- | Resynthesize sinusoidal and critical noise ATS analysis data.
atsNoiSynth :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atsNoiSynth b np ps pk fp sr nr m a nb bs bk = mkOsc AR "AtsNoiSynth" [b, np, ps, pk, fp, sr, nr, m, a, nb, bs, bk] 1

-- | Granular synthesis with FM grains.
fmGrain :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrain trigger dur carfreq modfreq ix = mkOsc AR "FMGrain" [trigger,dur,carfreq,modfreq,ix] 1

-- | Granular synthesis with FM grains and user supplied envelope.
fmGrainB :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainB trigger dur carfreq modfreq ix e = mkOsc AR "FMGrain" [trigger,dur,carfreq,modfreq,ix,e] 1

-- | Resynthesize LPC analysis data.
lpcSynth :: UGen -> UGen -> UGen -> UGen
lpcSynth b s ptr = mkOsc AR "LPCSynth" [b, s, ptr] 1

-- | Extract cps, rmso and err signals from LPC data.
lpcVals :: Rate -> UGen -> UGen -> UGen
lpcVals r b ptr = mkOsc r "LPCVals" [b, ptr] 3

-- | Metronome
metro :: Rate -> UGen -> UGen -> UGen
metro rt bpm nb = mkOsc rt "Metro" [bpm,nb] 1

-- | Invert FFT amplitude data.
pv_Invert :: UGen -> UGen
pv_Invert b = mkOsc KR "PV_Invert" [b] 1

-- * Membrane

-- | Triangular waveguide mesh of a drum-like membrane.
membraneCircle :: UGen -> UGen -> UGen -> UGen
membraneCircle i t l = mkOsc AR "MembraneCircle" [i, t, l] 1

-- | Triangular waveguide mesh of a drum-like membrane.
membraneHexagon :: UGen -> UGen -> UGen -> UGen
membraneHexagon i t l = mkOsc AR "MembraneHexagon" [i, t, l] 1

-- * NCAnalysisUGens

-- | Tracking Phase Vocoder
tpv :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tpv chain windowsize hopsize maxpeaks currentpeaks freqmult tolerance noisefloor = mkOsc AR "TPV" [chain,windowsize,hopsize,maxpeaks,currentpeaks,freqmult,tolerance,noisefloor] 1

-- * Stk

-- | STK bowed string model.
stkBowed :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkBowed rt f pr po vf vg l g at dc = mkOsc rt "StkBowed" [f, pr, po, vf, vg, l, g, at, dc] 1

-- | STK flute model.
stkFlute :: Rate-> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkFlute rt f jd ng vf vg bp tr = mkOsc rt "StkFlute" [f, jd, ng, vf, vg, bp, tr] 1

-- | STK mandolin model.
stkMandolin :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkMandolin rt f bs pp dm dt at tr = mkOsc rt "StkMandolin" [f, bs, pp, dm, dt, at, tr] 1

-- | STK modal bar models.
stkModalBar :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkModalBar rt f i sh sp vg vf mx v tr = mkOsc rt "StkModalBar" [f, i, sh, sp, vg, vf, mx, v, tr] 1

-- | STK shaker models.
stkShakers :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkShakers rt i e d o rf tr = mkOsc rt "StkShakers" [i, e, d, o, rf, tr] 1

-- * VOSIM

-- | Vocal simulation due to W. Kaegi.
vosim :: UGen -> UGen -> UGen -> UGen -> UGen
vosim t f nc d = mkOsc AR "VOSIM" [t, f, nc, d] 1


-- Local Variables:
-- truncate-lines:t
-- End:
