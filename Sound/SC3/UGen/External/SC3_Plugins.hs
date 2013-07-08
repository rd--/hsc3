-- | Bindings to unit generators in sc3-plugins.
module Sound.SC3.UGen.External.SC3_Plugins where

import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- * AntiAliasingOscillators (Nick Collins)

-- | Band limited impulse generation
blitB3 :: Rate -> UGen -> UGen
blitB3 rate freq = mkOscR [AR] rate "BlitB3" [freq] 1

-- | BLIT derived sawtooth
blitB3Saw :: Rate -> UGen -> UGen -> UGen
blitB3Saw rate freq leak = mkOscR [AR] rate "BlitB3Saw" [freq,leak] 1

-- | Bipolar BLIT derived square waveform
blitB3Square :: Rate -> UGen -> UGen -> UGen
blitB3Square rate freq leak = mkOscR [AR] rate "BlitB3Square" [freq,leak] 1

-- | Bipolar BLIT derived triangle
blitB3Tri :: Rate -> UGen -> UGen -> UGen -> UGen
blitB3Tri rate freq leak leak2 = mkOscR [AR] rate "BlitB3Tri" [freq,leak,leak2] 1

-- | Triangle via 3rd order differerentiated polynomial waveform
dPW3Tri :: Rate -> UGen -> UGen
dPW3Tri rate freq = mkOscR [AR] rate "DPW3Tri" [freq] 1

-- | Sawtooth via 4th order differerentiated polynomial waveform
dPW4Saw :: Rate -> UGen -> UGen
dPW4Saw rate freq = mkOscR [AR] rate "DPW4Saw" [freq] 1

-- * AY

-- | Emulation of AY (aka YM) soundchip, used in Spectrum\/Atari.
ay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
ay ta tb tc n c va vb vc ef es ct = mkOsc AR "AY" [ta, tb, tc, n, c, va, vb, vc, ef, es, ct] 1

-- | Convert frequency value to value appropriate for AY tone inputs.
ayFreqToTone :: Fractional a => a -> a
ayFreqToTone f = 110300 / (f - 0.5)

-- * BatUGens

-- | An amplitude tracking based onset detector
coyote :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
coyote rate in_ trackFall slowLag fastLag fastMul thresh minDur = mkOscR [KR] rate "Coyote" [in_,trackFall,slowLag,fastLag,fastMul,thresh,minDur] 1

-- * BhobUGens

-- | Impulses around a certain frequency
gaussTrig :: Rate -> UGen -> UGen -> UGen
gaussTrig rate freq dev = mkOscR [AR,KR] rate "GaussTrig" [freq,dev] 1

-- | String resonance filter
streson :: UGen -> UGen -> UGen -> UGen
streson input delayTime res = mkFilter "Streson" [input,delayTime,res] 1

-- | Triggered beta random distribution
tBetaRand :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tBetaRand z lo hi prob1 prob2 trig_ = mkFilterIdR [AR,KR] z "TBetaRand" [lo,hi,prob1,prob2,trig_] 1

-- | Triggered random walk generator
tBrownRand :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tBrownRand z lo hi dev dist trig_ = mkFilterIdR [AR,KR] z "TBrownRand" [lo,hi,dev,dist,trig_] 1

-- | Triggered gaussian random distribution
tGaussRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tGaussRand z lo hi trig_ = mkFilterIdR [AR,KR] z "TGaussRand" [lo,hi,trig_] 1

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

-- * DWGUGens

-- | Plucked physical model.
dWGPlucked2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dWGPlucked2 rate freq amp gate_ pos c1 c3 inp release mistune mp gc = mkOscR [AR] rate "DWGPlucked2" [freq,amp,gate_,pos,c1,c3,inp,release,mistune,mp,gc] 1

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

pv_BinDelay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinDelay buffer maxdelay delaybuf fbbuf hop = mkOsc KR "PV_BinDelay" [buffer,maxdelay,delaybuf,fbbuf,hop] 1

-- | Invert FFT amplitude data.
pv_Invert :: UGen -> UGen
pv_Invert b = mkOsc KR "PV_Invert" [b] 1

-- | Sample looping oscillator
loopBuf :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
loopBuf numChannels rate bufnum rate_ gate_ startPos startLoop endLoop interpolation = mkOscR [AR] rate "LoopBuf" [bufnum,rate_,gate_,startPos,startLoop,endLoop,interpolation] numChannels

-- * MCLD

-- | 3D Perlin Noise
perlin3 :: Rate -> UGen -> UGen -> UGen -> UGen
perlin3 rate x y z = mkOscR [AR,KR] rate "Perlin3" [x,y,z] 1

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

-- * SLU

-- | Prigogine oscillator
brusselator :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
brusselator rate reset rate_ mu gamma initx inity = mkOscR [AR] rate "Brusselator" [reset,rate_,mu,gamma,initx,inity] 2

-- | Forced DoubleWell Oscillator
doubleWell3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleWell3 rate reset rate_ f delta initx inity = mkOscR [AR] rate "DoubleWell3" [reset,rate_,f,delta,initx,inity] 1

-- | Envelope Follower Filter
envDetect :: Rate -> UGen -> UGen -> UGen -> UGen
envDetect rate in_ attack release = mkOscR [AR] rate "EnvDetect" [in_,attack,release] 1

-- | Envelope Follower
envFollow :: Rate -> UGen -> UGen -> UGen
envFollow rate input decaycoeff = mkOscR [AR,KR] rate "EnvFollow" [input,decaycoeff] 1

-- | Experimental time domain onset detector
sLOnset :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sLOnset rate input memorysize1 before after threshold hysteresis = mkOscR [KR] rate "SLOnset" [input,memorysize1,before,after,threshold,hysteresis] 1

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
