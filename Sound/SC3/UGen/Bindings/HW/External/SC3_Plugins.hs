-- | Bindings to unit generators in sc3-plugins.
module Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins where

import Sound.SC3.UGen.Bindings.HW.Construct
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type

-- * AntiAliasingOscillators (Nick Collins)

-- | Band limited impulse generation
blitB3 :: Rate -> UGen -> UGen
blitB3 rate freq = mkOscR [AR] rate "BlitB3" [freq] 1

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

-- | Convert frequency value to value appropriate for AY tone inputs.
ayFreqToTone :: Fractional a => a -> a
ayFreqToTone f = 110300 / (f - 0.5)

-- * BatUGens

-- | Windowed amplitude follower
wAmp :: Rate -> UGen -> UGen -> UGen
wAmp rate in_ winSize = mkOscR [KR] rate "WAmp" [in_,winSize] 1

-- * BhobUGens

-- | Impulses around a certain frequency
gaussTrig :: Rate -> UGen -> UGen -> UGen
gaussTrig rate freq dev = mkOscR [AR,KR] rate "GaussTrig" [freq,dev] 1

-- * BlackRain

-- * Concat

-- | Concatenative cross-synthesis.
concat' :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
concat' ctl src sz sk sd ml fs zcr lms sc st rs = mkOsc AR "Concat" [ctl,src,sz,sk,sd,ml,fs,zcr,lms,sc,st,rs] 1

-- | Concatenative cross-synthesis (variant).
concat2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
concat2 ctl src sz sk sd ml fs zcr lms sc st rs th = mkOsc AR "Concat2" [ctl,src,sz,sk,sd,ml,fs,zcr,lms,sc,st,rs,th] 1

-- * DEIND UGens

-- * Distortion

-- * DWGUGens

-- * Josh

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

-- * Membrane

-- * NCAnalysisUGens

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

-- * SCMIRUGens

-- | Octave chroma band based representation of energy in a signal; Chromagram for nTET tuning systems with any base reference
chromagram :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
chromagram rate fft_ fftsize n tuningbase octaves integrationflag coeff = mkOscR [KR] rate "Chromagram" [fft_,fftsize,n,tuningbase,octaves,integrationflag,coeff] 1

-- * skUG

-- * SLU

-- | Prigogine oscillator
brusselator :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
brusselator rate reset rate_ mu gamma initx inity = mkOscR [AR] rate "Brusselator" [reset,rate_,mu,gamma,initx,inity] 2

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

-- * TJUGens

-- * VOSIM

-- Local Variables:
-- truncate-lines:t
-- End:
