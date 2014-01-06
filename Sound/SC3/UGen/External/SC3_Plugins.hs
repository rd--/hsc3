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

-- * AuditoryModeling

-- | Single gammatone filter
gammatone :: UGen -> UGen -> UGen -> UGen
gammatone input centrefrequency bandwidth = mkFilterR [AR] "Gammatone" [input,centrefrequency,bandwidth] 1

-- | Simple cochlear hair cell model
hairCell :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
hairCell input spontaneousrate boostrate restorerate loss = mkFilterR [AR,KR] "HairCell" [input,spontaneousrate,boostrate,restorerate,loss] 1

-- | Meddis cochlear hair cell model
meddis :: UGen -> UGen
meddis input = mkFilterR [AR,KR] "Meddis" [input] 1

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

-- | Windowed amplitude follower
wAmp :: Rate -> UGen -> UGen -> UGen
wAmp rate in_ winSize = mkOscR [KR] rate "WAmp" [in_,winSize] 1

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

-- | Delay and Feedback on a bin by bin basis.
pv_BinDelay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinDelay buffer maxdelay delaybuf fbbuf hop = mkOsc KR "PV_BinDelay" [buffer,maxdelay,delaybuf,fbbuf,hop] 1

-- | Play FFT data from a memory buffer.
pv_BufRd :: UGen -> UGen -> UGen -> UGen
pv_BufRd buffer playbuf_ point = mkOsc KR "PV_BufRd" [buffer,playbuf_,point] 1

-- | /dur/ and /hop/ are in seconds, /frameSize/ and /sampleRate/ in
-- frames, though the latter maybe fractional.
--
-- > pv_calcPVRecSize 4.2832879818594 1024 0.25 48000.0 == 823299
pv_calcPVRecSize :: Double -> Int -> Double -> Double -> Int
pv_calcPVRecSize dur frameSize hop sampleRate =
    let frameSize' = fromIntegral frameSize
        rawsize = ceiling ((dur * sampleRate) / frameSize') * frameSize
    in ceiling (fromIntegral rawsize * recip hop + 3)

-- | Invert FFT amplitude data.
pv_Invert :: UGen -> UGen
pv_Invert b = mkOsc KR "PV_Invert" [b] 1

-- | Plays FFT data from a memory buffer.
pv_PlayBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_PlayBuf buffer playbuf_ rate_ offset loop = mkOsc KR "PV_PlayBuf" [buffer,playbuf_,rate_,offset,loop] 1

-- | Records FFT data to a memory buffer.
pv_RecordBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RecordBuf buffer recbuf offset run loop hop wintype = mkOsc KR "PV_RecordBuf" [buffer,recbuf,offset,run,loop,hop,wintype] 1

-- | Sample looping oscillator
loopBuf :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
loopBuf numChannels rate bufnum rate_ gate_ startPos startLoop endLoop interpolation = mkOscR [AR] rate "LoopBuf" [bufnum,rate_,gate_,startPos,startLoop,endLoop,interpolation] numChannels

-- * MCLD

-- | Detect the largest value (and its position) in an array of UGens
arrayMax :: Rate -> UGen -> UGen
arrayMax rate array = mkOscR [AR,KR] rate "ArrayMax" [array] 2

-- | Detect the smallest value (and its position) in an array of UGens
arrayMin :: Rate -> UGen -> UGen
arrayMin rate array = mkOscR [AR,KR] rate "ArrayMin" [array] 2

-- | Detect the largest value (and its position) in an array of UGens
bufMax :: Rate -> UGen -> UGen -> UGen
bufMax rate bufnum gate_ = mkOscR [KR] rate "BufMax" [bufnum,gate_] 2

-- | Detect the largest value (and its position) in an array of UGens
bufMin :: Rate -> UGen -> UGen -> UGen
bufMin rate bufnum gate_ = mkOscR [KR] rate "BufMin" [bufnum,gate_] 2

-- | 3D Perlin Noise
perlin3 :: Rate -> UGen -> UGen -> UGen -> UGen
perlin3 rate x y z = mkOscR [AR,KR] rate "Perlin3" [x,y,z] 1

-- | Wave squeezer. Maybe a kind of pitch shifter.
squiz :: UGen -> UGen -> UGen -> UGen -> UGen
squiz in_ pitchratio zcperchunk memlen = mkFilterR [AR,KR] "Squiz" [in_,pitchratio,zcperchunk,memlen] 1

-- * Membrane

-- | Triangular waveguide mesh of a drum-like membrane.
membraneCircle :: UGen -> UGen -> UGen -> UGen
membraneCircle i t l = mkOsc AR "MembraneCircle" [i, t, l] 1

-- | Triangular waveguide mesh of a drum-like membrane.
membraneHexagon :: UGen -> UGen -> UGen -> UGen
membraneHexagon i t l = mkOsc AR "MembraneHexagon" [i, t, l] 1

-- * NCAnalysisUGens

-- | Spectral Modeling Synthesis
sms :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sms input maxpeaks currentpeaks tolerance noisefloor freqmult freqadd formantpreserve useifft ampmult graphicsbufnum = mkFilterR [AR] "SMS" [input,maxpeaks,currentpeaks,tolerance,noisefloor,freqmult,freqadd,formantpreserve,useifft,ampmult,graphicsbufnum] 2

-- | Tracking Phase Vocoder
tpv :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tpv chain windowsize hopsize maxpeaks currentpeaks freqmult tolerance noisefloor = mkOsc AR "TPV" [chain,windowsize,hopsize,maxpeaks,currentpeaks,freqmult,tolerance,noisefloor] 1

-- * PitchDetection

-- | Tartini model pitch tracker.
tartini ::  Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tartini r input threshold n k overlap smallCutoff = mkOscR [KR] r "Tartini" [input,threshold,n,k,overlap,smallCutoff] 2

-- | Constant Q transform pitch follower.
qitch ::  Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
qitch r input databufnum ampThreshold algoflag ampbufnum minfreq maxfreq = mkOscR [KR] r "Qitch" [input,databufnum,ampThreshold,algoflag,ampbufnum,minfreq,maxfreq] 2

-- * RFWUGens

-- | Calculates mean average of audio or control rate signal.
averageOutput :: UGen -> UGen -> UGen
averageOutput in_ trig_ = mkFilterR [KR,AR] "AverageOutput" [in_,trig_] 1

-- | Feedback delay line implementing switch-and-ramp buffer jumping.
switchDelay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
switchDelay in_ drylevel wetlevel delaytime delayfactor maxdelaytime = mkFilterR [AR] "SwitchDelay" [in_,drylevel,wetlevel,delaytime,delayfactor,maxdelaytime] 1

-- * SCMIRUGens

-- | Octave chroma band based representation of energy in a signal; Chromagram for nTET tuning systems with any base reference
chromagram :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
chromagram rate fft_ fftsize n tuningbase octaves integrationflag coeff = mkOscR [KR] rate "Chromagram" [fft_,fftsize,n,tuningbase,octaves,integrationflag,coeff] 1

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

-- | Linear Time Invariant General Filter Equation
lti :: Rate -> UGen -> UGen -> UGen -> UGen
lti rate input bufnuma bufnumb = mkOscR [AR] rate "LTI" [input,bufnuma,bufnumb] 1

-- | Experimental time domain onset detector
sLOnset :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sLOnset rate input memorysize1 before after threshold hysteresis = mkOscR [KR] rate "SLOnset" [input,memorysize1,before,after,threshold,hysteresis] 1

-- | wave terrain synthesis
waveTerrain :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
waveTerrain rate bufnum x y xsize ysize = mkOscR [AR] rate "WaveTerrain" [bufnum,x,y,xsize,ysize] 1

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

-- * TJUGens

-- | Digitally modelled analog filter
dfm1 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dfm1 i f r g ty nl = mkFilter "DFM1" [i,f,r,g,ty,nl] 1

-- * VOSIM

-- | Vocal simulation due to W. Kaegi.
vosim :: UGen -> UGen -> UGen -> UGen -> UGen
vosim t f nc d = mkOsc AR "VOSIM" [t, f, nc, d] 1


-- Local Variables:
-- truncate-lines:t
-- End:
