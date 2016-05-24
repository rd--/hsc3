module Sound.SC3.UGen.Bindings.DB where

import Sound.SC3.UGen.Envelope
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Audio to control rate converter.
--
--  A2K [KR] in=0.0
a2K :: UGen -> UGen
a2K in_ = mkUGen Nothing [KR] (Left KR) "A2K" [in_] Nothing 1 (Special 0) NoId

-- | FIXME: APF purpose.
--
--  APF [KR,AR] in=0.0 freq=440.0 radius=0.8;    FILTER: TRUE
apf :: UGen -> UGen -> UGen -> UGen
apf in_ freq radius = mkUGen Nothing [KR,AR] (Right [0]) "APF" [in_,freq,radius] Nothing 1 (Special 0) NoId

-- | All pass delay line with cubic interpolation.
--
--  AllpassC [KR,AR] in=0.0 maxdelaytime=0.2 delaytime=0.2 decaytime=1.0;    FILTER: TRUE
allpassC :: UGen -> UGen -> UGen -> UGen -> UGen
allpassC in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "AllpassC" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | All pass delay line with linear interpolation.
--
--  AllpassL [KR,AR] in=0.0 maxdelaytime=0.2 delaytime=0.2 decaytime=1.0;    FILTER: TRUE
allpassL :: UGen -> UGen -> UGen -> UGen -> UGen
allpassL in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "AllpassL" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | All pass delay line with no interpolation.
--
--  AllpassN [KR,AR] in=0.0 maxdelaytime=0.2 delaytime=0.2 decaytime=1.0;    FILTER: TRUE
allpassN :: UGen -> UGen -> UGen -> UGen -> UGen
allpassN in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "AllpassN" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Basic psychoacoustic amplitude compensation.
--
--  AmpComp [IR,KR,AR] freq=0.0 root=0.0 exp=0.3333
ampComp :: Rate -> UGen -> UGen -> UGen -> UGen
ampComp rate freq root exp_ = mkUGen Nothing [IR,KR,AR] (Left rate) "AmpComp" [freq,root,exp_] Nothing 1 (Special 0) NoId

-- | Basic psychoacoustic amplitude compensation (ANSI A-weighting curve).
--
--  AmpCompA [IR,KR,AR] freq=1000.0 root=0.0 minAmp=0.32 rootAmp=1.0
ampCompA :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
ampCompA rate freq root minAmp rootAmp = mkUGen Nothing [IR,KR,AR] (Left rate) "AmpCompA" [freq,root,minAmp,rootAmp] Nothing 1 (Special 0) NoId

-- | Amplitude follower
--
--  Amplitude [KR,AR] in=0.0 attackTime=1.0e-2 releaseTime=1.0e-2
amplitude :: Rate -> UGen -> UGen -> UGen -> UGen
amplitude rate in_ attackTime releaseTime = mkUGen Nothing [KR,AR] (Left rate) "Amplitude" [in_,attackTime,releaseTime] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AudioControl [AR] values=0.0
audioControl :: Rate -> UGen -> UGen
audioControl rate values = mkUGen Nothing [AR] (Left rate) "AudioControl" [values] Nothing 0 (Special 0) NoId

-- | All Pass Filter
--
--  BAllPass [AR] in=0.0 freq=1200.0 rq=1.0;    FILTER: TRUE
bAllPass :: UGen -> UGen -> UGen -> UGen
bAllPass in_ freq rq = mkUGen Nothing [AR] (Right [0]) "BAllPass" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Band Pass Filter
--
--  BBandPass [AR] in=0.0 freq=1200.0 bw=1.0;    FILTER: TRUE
bBandPass :: UGen -> UGen -> UGen -> UGen
bBandPass in_ freq bw = mkUGen Nothing [AR] (Right [0]) "BBandPass" [in_,freq,bw] Nothing 1 (Special 0) NoId

-- | Band reject filter
--
--  BBandStop [AR] in=0.0 freq=1200.0 bw=1.0;    FILTER: TRUE
bBandStop :: UGen -> UGen -> UGen -> UGen
bBandStop in_ freq bw = mkUGen Nothing [AR] (Right [0]) "BBandStop" [in_,freq,bw] Nothing 1 (Special 0) NoId

-- | 12db/oct rolloff - 2nd order resonant  Hi Pass Filter
--
--  BHiPass [AR] in=0.0 freq=1200.0 rq=1.0;    FILTER: TRUE
bHiPass :: UGen -> UGen -> UGen -> UGen
bHiPass in_ freq rq = mkUGen Nothing [AR] (Right [0]) "BHiPass" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Hi Shelf
--
--  BHiShelf [AR] in=0.0 freq=1200.0 rs=1.0 db=0.0;    FILTER: TRUE
bHiShelf :: UGen -> UGen -> UGen -> UGen -> UGen
bHiShelf in_ freq rs db = mkUGen Nothing [AR] (Right [0]) "BHiShelf" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | 12db/oct rolloff - 2nd order resonant Low Pass Filter
--
--  BLowPass [AR] in=0.0 freq=1200.0 rq=1.0;    FILTER: TRUE
bLowPass :: UGen -> UGen -> UGen -> UGen
bLowPass in_ freq rq = mkUGen Nothing [AR] (Right [0]) "BLowPass" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Low Shelf
--
--  BLowShelf [AR] in=0.0 freq=1200.0 rs=1.0 db=0.0;    FILTER: TRUE
bLowShelf :: UGen -> UGen -> UGen -> UGen -> UGen
bLowShelf in_ freq rs db = mkUGen Nothing [AR] (Right [0]) "BLowShelf" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | 2nd order Butterworth bandpass filter.
--
--  BPF [KR,AR] in=0.0 freq=440.0 rq=1.0;    FILTER: TRUE
bpf :: UGen -> UGen -> UGen -> UGen
bpf in_ freq rq = mkUGen Nothing [KR,AR] (Right [0]) "BPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Two zero fixed midpass.
--
--  BPZ2 [KR,AR] in=0.0;    FILTER: TRUE
bpz2 :: UGen -> UGen
bpz2 in_ = mkUGen Nothing [KR,AR] (Right [0]) "BPZ2" [in_] Nothing 1 (Special 0) NoId

-- | Parametric equalizer
--
--  BPeakEQ [AR] in=0.0 freq=1200.0 rq=1.0 db=0.0;    FILTER: TRUE
bPeakEQ :: UGen -> UGen -> UGen -> UGen -> UGen
bPeakEQ in_ freq rq db = mkUGen Nothing [AR] (Right [0]) "BPeakEQ" [in_,freq,rq,db] Nothing 1 (Special 0) NoId

-- | 2nd order Butterworth band reject filter.
--
--  BRF [KR,AR] in=0.0 freq=440.0 rq=1.0;    FILTER: TRUE
brf :: UGen -> UGen -> UGen -> UGen
brf in_ freq rq = mkUGen Nothing [KR,AR] (Right [0]) "BRF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Two zero fixed midcut.
--
--  BRZ2 [KR,AR] in=0.0;    FILTER: TRUE
brz2 :: UGen -> UGen
brz2 in_ = mkUGen Nothing [KR,AR] (Right [0]) "BRZ2" [in_] Nothing 1 (Special 0) NoId

-- | Stereo signal balancer
--
--  Balance2 [KR,AR] left=0.0 right=0.0 pos=0.0 level=1.0;    FILTER: TRUE
balance2 :: UGen -> UGen -> UGen -> UGen -> UGen
balance2 left right pos level = mkUGen Nothing [KR,AR] (Right [0,1]) "Balance2" [left,right,pos,level] Nothing 2 (Special 0) NoId

-- | physical model of bouncing object
--
--  Ball [KR,AR] in=0.0 g=1.0 damp=0.0 friction=1.0e-2
ball :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
ball rate in_ g damp friction = mkUGen Nothing [KR,AR] (Left rate) "Ball" [in_,g,damp,friction] Nothing 1 (Special 0) NoId

-- | Autocorrelation beat tracker
--
--  BeatTrack [KR] chain=0.0 lock=0.0
beatTrack :: Rate -> UGen -> UGen -> UGen
beatTrack rate chain lock = mkUGen Nothing [KR] (Left rate) "BeatTrack" [chain,lock] Nothing 4 (Special 0) NoId

-- | Template matching beat tracker
--
--  BeatTrack2 [KR] busindex=0.0 numfeatures=0.0 windowsize=2.0 phaseaccuracy=2.0e-2 lock=0.0 weightingscheme=0.0
beatTrack2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
beatTrack2 rate busindex numfeatures windowsize phaseaccuracy lock weightingscheme = mkUGen Nothing [KR] (Left rate) "BeatTrack2" [busindex,numfeatures,windowsize,phaseaccuracy,lock,weightingscheme] Nothing 6 (Special 0) NoId

-- | 2D Ambisonic B-format panner.
--
--  BiPanB2 [KR,AR] inA=0.0 inB=0.0 azimuth=0.0 gain=1.0
biPanB2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
biPanB2 rate inA inB azimuth gain = mkUGen Nothing [KR,AR] (Left rate) "BiPanB2" [inA,inB,azimuth,gain] Nothing 3 (Special 0) NoId

-- | Apply a binary operation to the values of an input UGen
--
--  BinaryOpUGen [] a=0.0 b=0.0;    FILTER: TRUE
binaryOpUGen :: UGen -> UGen -> UGen
binaryOpUGen a b = mkUGen Nothing [IR,KR,AR,DR] (Right [0,1]) "BinaryOpUGen" [a,b] Nothing 1 (Special 0) NoId

-- | Band limited impulse oscillator.
--
--  Blip [KR,AR] freq=440.0 numharm=200.0
blip :: Rate -> UGen -> UGen -> UGen
blip rate freq numharm = mkUGen Nothing [KR,AR] (Left rate) "Blip" [freq,numharm] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BlockSize [IR] 
blockSize :: UGen
blockSize = mkUGen Nothing [IR] (Left IR) "BlockSize" [] Nothing 1 (Special 0) NoId

-- | Brown Noise.
--
--  BrownNoise [KR,AR] ;    NONDET
brownNoise :: ID a => a -> Rate -> UGen
brownNoise z rate = mkUGen Nothing [KR,AR] (Left rate) "BrownNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Buffer based all pass delay line with cubic interpolation.
--
--  BufAllpassC [AR] buf=0.0 in=0.0 delaytime=0.2 decaytime=1.0
bufAllpassC :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassC buf in_ delaytime decaytime = mkUGen Nothing [AR] (Right [1]) "BufAllpassC" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based all pass delay line with linear interpolation.
--
--  BufAllpassL [AR] buf=0.0 in=0.0 delaytime=0.2 decaytime=1.0
bufAllpassL :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassL buf in_ delaytime decaytime = mkUGen Nothing [AR] (Right [1]) "BufAllpassL" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based all pass delay line with no interpolation.
--
--  BufAllpassN [AR] buf=0.0 in=0.0 delaytime=0.2 decaytime=1.0
bufAllpassN :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassN buf in_ delaytime decaytime = mkUGen Nothing [AR] (Right [1]) "BufAllpassN" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Current number of channels of soundfile in buffer.
--
--  BufChannels [IR,KR] bufnum=0.0
bufChannels :: Rate -> UGen -> UGen
bufChannels rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufChannels" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer based comb delay line with cubic interpolation.
--
--  BufCombC [AR] buf=0.0 in=0.0 delaytime=0.2 decaytime=1.0;    FILTER: TRUE
bufCombC :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombC buf in_ delaytime decaytime = mkUGen Nothing [AR] (Right [1]) "BufCombC" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based comb delay line with linear interpolation.
--
--  BufCombL [AR] buf=0.0 in=0.0 delaytime=0.2 decaytime=1.0;    FILTER: TRUE
bufCombL :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombL buf in_ delaytime decaytime = mkUGen Nothing [AR] (Right [1]) "BufCombL" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based comb delay line with no interpolation.
--
--  BufCombN [AR] buf=0.0 in=0.0 delaytime=0.2 decaytime=1.0;    FILTER: TRUE
bufCombN :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombN buf in_ delaytime decaytime = mkUGen Nothing [AR] (Right [1]) "BufCombN" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based simple delay line with cubic interpolation.
--
--  BufDelayC [KR,AR] buf=0.0 in=0.0 delaytime=0.2
bufDelayC :: UGen -> UGen -> UGen -> UGen
bufDelayC buf in_ delaytime = mkUGen Nothing [KR,AR] (Right [1]) "BufDelayC" [buf,in_,delaytime] Nothing 1 (Special 0) NoId

-- | Buffer based simple delay line with linear interpolation.
--
--  BufDelayL [KR,AR] buf=0.0 in=0.0 delaytime=0.2
bufDelayL :: UGen -> UGen -> UGen -> UGen
bufDelayL buf in_ delaytime = mkUGen Nothing [KR,AR] (Right [1]) "BufDelayL" [buf,in_,delaytime] Nothing 1 (Special 0) NoId

-- | Buffer based simple delay line with no interpolation.
--
--  BufDelayN [KR,AR] buf=0.0 in=0.0 delaytime=0.2
bufDelayN :: UGen -> UGen -> UGen -> UGen
bufDelayN buf in_ delaytime = mkUGen Nothing [KR,AR] (Right [1]) "BufDelayN" [buf,in_,delaytime] Nothing 1 (Special 0) NoId

-- | Current duration of soundfile in buffer.
--
--  BufDur [IR,KR] bufnum=0.0
bufDur :: Rate -> UGen -> UGen
bufDur rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufDur" [bufnum] Nothing 1 (Special 0) NoId

-- | Current number of frames allocated in the buffer.
--
--  BufFrames [IR,KR] bufnum=0.0
bufFrames :: Rate -> UGen -> UGen
bufFrames rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufFrames" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer rate scaling in respect to server samplerate.
--
--  BufRateScale [IR,KR] bufnum=0.0
bufRateScale :: Rate -> UGen -> UGen
bufRateScale rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufRateScale" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer reading oscillator.
--
--  BufRd [KR,AR] bufnum=0.0 phase=0.0 loop=1.0 interpolation=2.0;    NC INPUT: True, ENUMERATION INPUTS: 2=Loop, 3=Interpolation
bufRd :: Int -> Rate -> UGen -> UGen -> Loop -> Interpolation -> UGen
bufRd numChannels rate bufnum phase loop interpolation = mkUGen Nothing [KR,AR] (Left rate) "BufRd" [bufnum,phase,(from_loop loop),(from_interpolation interpolation)] Nothing numChannels (Special 0) NoId

-- | Buffer sample rate.
--
--  BufSampleRate [IR,KR] bufnum=0.0
bufSampleRate :: Rate -> UGen -> UGen
bufSampleRate rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufSampleRate" [bufnum] Nothing 1 (Special 0) NoId

-- | Current number of samples in buffer.
--
--  BufSamples [IR,KR] bufnum=0.0
bufSamples :: Rate -> UGen -> UGen
bufSamples rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufSamples" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer writing oscillator.
--
--  BufWr [KR,AR] bufnum=0.0 phase=0.0 loop=1.0 *inputArray=0.0;    MCE, FILTER: TRUE, REORDERS INPUTS: [3,0,1,2], ENUMERATION INPUTS: 2=Loop
bufWr :: UGen -> UGen -> Loop -> UGen -> UGen
bufWr bufnum phase loop inputArray = mkUGen Nothing [KR,AR] (Right [3]) "BufWr" [bufnum,phase,(from_loop loop)] (Just inputArray) 1 (Special 0) NoId

-- | Chorusing wavetable oscillator.
--
--  COsc [KR,AR] bufnum=0.0 freq=440.0 beats=0.5
cOsc :: Rate -> UGen -> UGen -> UGen -> UGen
cOsc rate bufnum freq beats = mkUGen Nothing [KR,AR] (Left rate) "COsc" [bufnum,freq,beats] Nothing 1 (Special 0) NoId

-- | Test for infinity, not-a-number, and denormals
--
--  CheckBadValues [KR,AR] in=0.0 id=0.0 post=2.0;    FILTER: TRUE
checkBadValues :: UGen -> UGen -> UGen -> UGen
checkBadValues in_ id_ post = mkUGen Nothing [KR,AR] (Right [0]) "CheckBadValues" [in_,id_,post] Nothing 1 (Special 0) NoId

-- | Clip a signal outside given thresholds.
--
--  Clip [IR,KR,AR] in=0.0 lo=0.0 hi=1.0;    FILTER: TRUE
clip :: UGen -> UGen -> UGen -> UGen
clip in_ lo hi = mkUGen Nothing [IR,KR,AR] (Right [0]) "Clip" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Clip Noise.
--
--  ClipNoise [KR,AR] ;    NONDET
clipNoise :: ID a => a -> Rate -> UGen
clipNoise z rate = mkUGen Nothing [KR,AR] (Left rate) "ClipNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Statistical gate.
--
--  CoinGate [KR,AR] prob=0.0 in=0.0;    FILTER: TRUE, NONDET
coinGate :: ID a => a -> UGen -> UGen -> UGen
coinGate z prob in_ = mkUGen Nothing [KR,AR] (Right [1]) "CoinGate" [prob,in_] Nothing 1 (Special 0) (toUId z)

-- | Comb delay line with cubic interpolation.
--
--  CombC [KR,AR] in=0.0 maxdelaytime=0.2 delaytime=0.2 decaytime=1.0;    FILTER: TRUE
combC :: UGen -> UGen -> UGen -> UGen -> UGen
combC in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "CombC" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Comb delay line with linear interpolation.
--
--  CombL [KR,AR] in=0.0 maxdelaytime=0.2 delaytime=0.2 decaytime=1.0;    FILTER: TRUE
combL :: UGen -> UGen -> UGen -> UGen -> UGen
combL in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "CombL" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Comb delay line with no interpolation.
--
--  CombN [KR,AR] in=0.0 maxdelaytime=0.2 delaytime=0.2 decaytime=1.0;    FILTER: TRUE
combN :: UGen -> UGen -> UGen -> UGen -> UGen
combN in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "CombN" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Compressor, expander, limiter, gate, ducker
--
--  Compander [AR] in=0.0 control=0.0 thresh=0.5 slopeBelow=1.0 slopeAbove=1.0 clampTime=1.0e-2 relaxTime=0.1;    FILTER: TRUE
compander :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
compander in_ control_ thresh slopeBelow slopeAbove clampTime relaxTime = mkUGen Nothing [AR] (Right [0]) "Compander" [in_,control_,thresh,slopeBelow,slopeAbove,clampTime,relaxTime] Nothing 1 (Special 0) NoId

-- | Compressor, expander, limiter, gate, ducker.
--
--  CompanderD [AR] in=0.0 thresh=0.5 slopeBelow=1.0 slopeAbove=1.0 clampTime=1.0e-2 relaxTime=1.0e-2
companderD :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
companderD rate in_ thresh slopeBelow slopeAbove clampTime relaxTime = mkUGen Nothing [AR] (Left rate) "CompanderD" [in_,thresh,slopeBelow,slopeAbove,clampTime,relaxTime] Nothing 1 (Special 0) NoId

-- | Duration of one block
--
--  ControlDur [IR] 
controlDur :: UGen
controlDur = mkUGen Nothing [IR] (Left IR) "ControlDur" [] Nothing 1 (Special 0) NoId

-- | Server control rate.
--
--  ControlRate [IR] 
controlRate :: UGen
controlRate = mkUGen Nothing [IR] (Left IR) "ControlRate" [] Nothing 1 (Special 0) NoId

-- | Real-time convolver.
--
--  Convolution [AR] in=0.0 kernel=0.0 framesize=512.0
convolution :: Rate -> UGen -> UGen -> UGen -> UGen
convolution rate in_ kernel framesize = mkUGen Nothing [AR] (Left rate) "Convolution" [in_,kernel,framesize] Nothing 1 (Special 0) NoId

-- | Real-time fixed kernel convolver.
--
--  Convolution2 [AR] in=0.0 kernel=0.0 trigger=0.0 framesize=2048.0
convolution2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
convolution2 rate in_ kernel trigger framesize = mkUGen Nothing [AR] (Left rate) "Convolution2" [in_,kernel,trigger,framesize] Nothing 1 (Special 0) NoId

-- | Real-time convolver with linear interpolation
--
--  Convolution2L [AR] in=0.0 kernel=0.0 trigger=0.0 framesize=2048.0 crossfade=1.0
convolution2L :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
convolution2L rate in_ kernel trigger framesize crossfade = mkUGen Nothing [AR] (Left rate) "Convolution2L" [in_,kernel,trigger,framesize,crossfade] Nothing 1 (Special 0) NoId

-- | Time based convolver.
--
--  Convolution3 [KR,AR] in=0.0 kernel=0.0 trigger=0.0 framesize=2048.0
convolution3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
convolution3 rate in_ kernel trigger framesize = mkUGen Nothing [KR,AR] (Left rate) "Convolution3" [in_,kernel,trigger,framesize] Nothing 1 (Special 0) NoId

-- | Chaotic noise function.
--
--  Crackle [KR,AR] chaosParam=1.5
crackle :: Rate -> UGen -> UGen
crackle rate chaosParam = mkUGen Nothing [KR,AR] (Left rate) "Crackle" [chaosParam] Nothing 1 (Special 0) NoId

-- | Cusp map chaotic generator
--
--  CuspL [AR] freq=22050.0 a=1.0 b=1.9 xi=0.0
cuspL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
cuspL rate freq a b xi = mkUGen Nothing [AR] (Left rate) "CuspL" [freq,a,b,xi] Nothing 1 (Special 0) NoId

-- | Cusp map chaotic generator
--
--  CuspN [AR] freq=22050.0 a=1.0 b=1.9 xi=0.0
cuspN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
cuspN rate freq a b xi = mkUGen Nothing [AR] (Left rate) "CuspN" [freq,a,b,xi] Nothing 1 (Special 0) NoId

-- | Create a constant amplitude signal
--
--  DC [KR,AR] in=0.0
dc :: Rate -> UGen -> UGen
dc rate in_ = mkUGen Nothing [KR,AR] (Left rate) "DC" [in_] Nothing 1 (Special 0) NoId

-- | Demand rate brownian movement generator.
--
--  Dbrown [DR] length=1.0e8 lo=0.0 hi=1.0 step=1.0e-2;    REORDERS INPUTS: [1,2,3,0], DEMAND/NONDET
dbrown :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
dbrown z length_ lo hi step = mkUGen Nothing [DR] (Left DR) "Dbrown" [length_,lo,hi,step] Nothing 1 (Special 0) (toUId z)

-- | Buffer read demand ugen
--
--  Dbufrd [DR] bufnum=0.0 phase=0.0 loop=1.0;    ENUMERATION INPUTS: 2=Loop, DEMAND/NONDET
dbufrd :: ID a => a -> UGen -> UGen -> Loop -> UGen
dbufrd z bufnum phase loop = mkUGen Nothing [DR] (Left DR) "Dbufrd" [bufnum,phase,(from_loop loop)] Nothing 1 (Special 0) (toUId z)

-- | Buffer write demand ugen
--
--  Dbufwr [DR] bufnum=0.0 phase=0.0 loop=1.0 input=0.0;    REORDERS INPUTS: [3,0,1,2], ENUMERATION INPUTS: 3=Loop, DEMAND/NONDET
dbufwr :: ID a => a -> UGen -> UGen -> UGen -> Loop -> UGen
dbufwr z bufnum phase loop input = mkUGen Nothing [DR] (Left DR) "Dbufwr" [bufnum,phase,loop,(from_loop input)] Nothing 1 (Special 0) (toUId z)

-- | Exponential decay
--
--  Decay [KR,AR] in=0.0 decayTime=1.0;    FILTER: TRUE
decay :: UGen -> UGen -> UGen
decay in_ decayTime = mkUGen Nothing [KR,AR] (Right [0]) "Decay" [in_,decayTime] Nothing 1 (Special 0) NoId

-- | Exponential decay
--
--  Decay2 [KR,AR] in=0.0 attackTime=1.0e-2 decayTime=1.0;    FILTER: TRUE
decay2 :: UGen -> UGen -> UGen -> UGen
decay2 in_ attackTime decayTime = mkUGen Nothing [KR,AR] (Right [0]) "Decay2" [in_,attackTime,decayTime] Nothing 1 (Special 0) NoId

-- | 2D Ambisonic B-format decoder.
--
--  DecodeB2 [KR,AR] w=0.0 x=0.0 y=0.0 orientation=0.5;    NC INPUT: True
decodeB2 :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen
decodeB2 numChannels rate w x y orientation = mkUGen Nothing [KR,AR] (Left rate) "DecodeB2" [w,x,y,orientation] Nothing numChannels (Special 0) NoId

-- | Convert signal to modal pitch.
--
--  DegreeToKey [KR,AR] bufnum=0.0 in=0.0 octave=12.0;    FILTER: TRUE
degreeToKey :: UGen -> UGen -> UGen -> UGen
degreeToKey bufnum in_ octave = mkUGen Nothing [KR,AR] (Right [1]) "DegreeToKey" [bufnum,in_,octave] Nothing 1 (Special 0) NoId

-- | Tap a delay line from a DelTapWr UGen
--
--  DelTapRd [KR,AR] buffer=0.0 phase=0.0 delTime=0.0 interp=1.0;    FILTER: TRUE
delTapRd :: UGen -> UGen -> UGen -> UGen -> UGen
delTapRd buffer phase delTime interp = mkUGen Nothing [KR,AR] (Right [1]) "DelTapRd" [buffer,phase,delTime,interp] Nothing 1 (Special 0) NoId

-- | Write to a buffer for a DelTapRd UGen
--
--  DelTapWr [KR,AR] buffer=0.0 in=0.0;    FILTER: TRUE
delTapWr :: UGen -> UGen -> UGen
delTapWr buffer in_ = mkUGen Nothing [KR,AR] (Right [1]) "DelTapWr" [buffer,in_] Nothing 1 (Special 0) NoId

-- | Single sample delay.
--
--  Delay1 [KR,AR] in=0.0;    FILTER: TRUE
delay1 :: UGen -> UGen
delay1 in_ = mkUGen Nothing [KR,AR] (Right [0]) "Delay1" [in_] Nothing 1 (Special 0) NoId

-- | Two sample delay.
--
--  Delay2 [KR,AR] in=0.0;    FILTER: TRUE
delay2 :: UGen -> UGen
delay2 in_ = mkUGen Nothing [KR,AR] (Right [0]) "Delay2" [in_] Nothing 1 (Special 0) NoId

-- | Simple delay line with cubic interpolation.
--
--  DelayC [KR,AR] in=0.0 maxdelaytime=0.2 delaytime=0.2;    FILTER: TRUE
delayC :: UGen -> UGen -> UGen -> UGen
delayC in_ maxdelaytime delaytime = mkUGen Nothing [KR,AR] (Right [0]) "DelayC" [in_,maxdelaytime,delaytime] Nothing 1 (Special 0) NoId

-- | Simple delay line with linear interpolation.
--
--  DelayL [KR,AR] in=0.0 maxdelaytime=0.2 delaytime=0.2;    FILTER: TRUE
delayL :: UGen -> UGen -> UGen -> UGen
delayL in_ maxdelaytime delaytime = mkUGen Nothing [KR,AR] (Right [0]) "DelayL" [in_,maxdelaytime,delaytime] Nothing 1 (Special 0) NoId

-- | Simple delay line with no interpolation.
--
--  DelayN [KR,AR] in=0.0 maxdelaytime=0.2 delaytime=0.2;    FILTER: TRUE
delayN :: UGen -> UGen -> UGen -> UGen
delayN in_ maxdelaytime delaytime = mkUGen Nothing [KR,AR] (Right [0]) "DelayN" [in_,maxdelaytime,delaytime] Nothing 1 (Special 0) NoId

-- | Demand results from demand rate UGens.
--
--  Demand [KR,AR] trig=0.0 reset=0.0 *demandUGens=0.0;    MCE, FILTER: TRUE
demand :: UGen -> UGen -> UGen -> UGen
demand trig_ reset demandUGens = mkUGen Nothing [KR,AR] (Right [0]) "Demand" [trig_,reset] (Just demandUGens) (length (mceChannels demandUGens) + 0) (Special 0) NoId

-- | Demand rate envelope generator
--
--  DemandEnvGen [KR,AR] level=0.0 dur=0.0 shape=1.0 curve=0.0 gate=1.0 reset=1.0 levelScale=1.0 levelBias=0.0 timeScale=1.0 doneAction=0.0;    ENUMERATION INPUTS: 9=DoneAction
demandEnvGen :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> DoneAction -> UGen
demandEnvGen rate level dur shape curve gate_ reset levelScale levelBias timeScale doneAction = mkUGen Nothing [KR,AR] (Left rate) "DemandEnvGen" [level,dur,shape,curve,gate_,reset,levelScale,levelBias,timeScale,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Search a buffer for a value
--
--  DetectIndex [KR,AR] bufnum=0.0 in=0.0;    FILTER: TRUE
detectIndex :: UGen -> UGen -> UGen
detectIndex bufnum in_ = mkUGen Nothing [KR,AR] (Right [1]) "DetectIndex" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | When input falls below a threshhold, evaluate doneAction.
--
--  DetectSilence [KR,AR] in=0.0 amp=1.0e-4 time=0.1 doneAction=0.0;    FILTER: TRUE, ENUMERATION INPUTS: 3=DoneAction
detectSilence :: UGen -> UGen -> UGen -> DoneAction -> UGen
detectSilence in_ amp time doneAction = mkUGen Nothing [KR,AR] (Right [0]) "DetectSilence" [in_,amp,time,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Demand rate geometric series UGen.
--
--  Dgeom [DR] length=1.0e8 start=1.0 grow=2.0;    REORDERS INPUTS: [1,2,0], DEMAND/NONDET
dgeom :: ID a => a -> UGen -> UGen -> UGen -> UGen
dgeom z length_ start grow = mkUGen Nothing [DR] (Left DR) "Dgeom" [length_,start,grow] Nothing 1 (Special 0) (toUId z)

-- | Demand rate brownian movement generator.
--
--  Dibrown [DR] length=1.0e8 lo=0.0 hi=1.0 step=1.0e-2;    REORDERS INPUTS: [1,2,3,0], DEMAND/NONDET
dibrown :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
dibrown z length_ lo hi step = mkUGen Nothing [DR] (Left DR) "Dibrown" [length_,lo,hi,step] Nothing 1 (Special 0) (toUId z)

-- | Stream in audio from a file.
--
--  DiskIn [AR] bufnum=0.0 loop=0.0;    NC INPUT: True, ENUMERATION INPUTS: 1=Loop
diskIn :: Int -> UGen -> Loop -> UGen
diskIn numChannels bufnum loop = mkUGen Nothing [AR] (Left AR) "DiskIn" [bufnum,(from_loop loop)] Nothing numChannels (Special 0) NoId

-- | Record to a soundfile to disk.
--
--  DiskOut [AR] bufnum=0.0 *channelsArray=0.0;    MCE
diskOut :: UGen -> UGen -> UGen
diskOut bufnum input = mkUGen Nothing [AR] (Left AR) "DiskOut" [bufnum] (Just input) 1 (Special 0) NoId

-- | Demand rate white noise random generator.
--
--  Diwhite [DR] length=1.0e8 lo=0.0 hi=1.0;    REORDERS INPUTS: [1,2,0], DEMAND/NONDET
diwhite :: ID a => a -> UGen -> UGen -> UGen -> UGen
diwhite z length_ lo hi = mkUGen Nothing [DR] (Left DR) "Diwhite" [length_,lo,hi] Nothing 1 (Special 0) (toUId z)

-- | (Undocumented class)
--
--  Donce [DR] in=0.0;    DEMAND/NONDET
donce :: ID a => a -> UGen -> UGen
donce z in_ = mkUGen Nothing [DR] (Left DR) "Donce" [in_] Nothing 1 (Special 0) (toUId z)

-- | Monitors another UGen to see when it is finished
--
--  Done [KR] src=0.0
done :: UGen -> UGen
done src = mkUGen Nothing [KR] (Left KR) "Done" [src] Nothing 1 (Special 0) NoId

-- | Print the current output value of a demand rate UGen
--
--  Dpoll [DR] in=0.0 label=0.0 run=1.0 trigid=-1.0;    DEMAND/NONDET
dpoll :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
dpoll z in_ label_ run trigid = mkUGen Nothing [DR] (Left DR) "Dpoll" [in_,label_,run,trigid] Nothing 1 (Special 0) (toUId z)

-- | Demand rate random sequence generator.
--
--  Drand [DR] repeats=1.0 *list=0.0;    MCE, REORDERS INPUTS: [1,0], DEMAND/NONDET
drand :: ID a => a -> UGen -> UGen -> UGen
drand z repeats list_ = mkUGen Nothing [DR] (Left DR) "Drand" [repeats] (Just list_) 1 (Special 0) (toUId z)

-- | demand rate reset
--
--  Dreset [DR] in=0.0 reset=0.0;    DEMAND/NONDET
dreset :: ID a => a -> UGen -> UGen -> UGen
dreset z in_ reset = mkUGen Nothing [DR] (Left DR) "Dreset" [in_,reset] Nothing 1 (Special 0) (toUId z)

-- | Demand rate sequence generator.
--
--  Dseq [DR] repeats=1.0 *list=0.0;    MCE, REORDERS INPUTS: [1,0], DEMAND/NONDET
dseq :: ID a => a -> UGen -> UGen -> UGen
dseq z repeats list_ = mkUGen Nothing [DR] (Left DR) "Dseq" [repeats] (Just list_) 1 (Special 0) (toUId z)

-- | Demand rate sequence generator.
--
--  Dser [DR] repeats=1.0 *list=0.0;    MCE, REORDERS INPUTS: [1,0], DEMAND/NONDET
dser :: ID a => a -> UGen -> UGen -> UGen
dser z repeats list_ = mkUGen Nothing [DR] (Left DR) "Dser" [repeats] (Just list_) 1 (Special 0) (toUId z)

-- | Demand rate arithmetic series UGen.
--
--  Dseries [DR] length=1.0e8 start=1.0 step=1.0;    REORDERS INPUTS: [1,2,0], DEMAND/NONDET
dseries :: ID a => a -> UGen -> UGen -> UGen -> UGen
dseries z length_ start step = mkUGen Nothing [DR] (Left DR) "Dseries" [length_,start,step] Nothing 1 (Special 0) (toUId z)

-- | Demand rate random sequence generator
--
--  Dshuf [DR] repeats=1.0 *list=0.0;    MCE, REORDERS INPUTS: [1,0], DEMAND/NONDET
dshuf :: ID a => a -> UGen -> UGen -> UGen
dshuf z repeats list_ = mkUGen Nothing [DR] (Left DR) "Dshuf" [repeats] (Just list_) 1 (Special 0) (toUId z)

-- | Demand rate input replicator
--
--  Dstutter [DR] n=0.0 in=0.0;    DEMAND/NONDET
dstutter :: ID a => a -> UGen -> UGen -> UGen
dstutter z n in_ = mkUGen Nothing [DR] (Left DR) "Dstutter" [n,in_] Nothing 1 (Special 0) (toUId z)

-- | Demand rate generator for embedding different inputs
--
--  Dswitch [DR] index=0.0 *list=0.0;    MCE, REORDERS INPUTS: [1,0], DEMAND/NONDET
dswitch :: ID a => a -> UGen -> UGen -> UGen
dswitch z index_ list_ = mkUGen Nothing [DR] (Left DR) "Dswitch" [index_] (Just list_) 1 (Special 0) (toUId z)

-- | Demand rate generator for switching between inputs.
--
--  Dswitch1 [DR] index=0.0 *list=0.0;    MCE, REORDERS INPUTS: [1,0], DEMAND/NONDET
dswitch1 :: ID a => a -> UGen -> UGen -> UGen
dswitch1 z index_ list_ = mkUGen Nothing [DR] (Left DR) "Dswitch1" [index_] (Just list_) 1 (Special 0) (toUId z)

-- | Return the same unique series of values for several demand streams
--
--  Dunique [DR] source=0.0 maxBufferSize=1024.0 protected=1.0;    DEMAND/NONDET
dunique :: ID a => a -> UGen -> UGen -> UGen -> UGen
dunique z source maxBufferSize protected = mkUGen Nothing [DR] (Left DR) "Dunique" [source,maxBufferSize,protected] Nothing 1 (Special 0) (toUId z)

-- | Random impulses.
--
--  Dust [KR,AR] density=0.0;    NONDET
dust :: ID a => a -> Rate -> UGen -> UGen
dust z rate density = mkUGen Nothing [KR,AR] (Left rate) "Dust" [density] Nothing 1 (Special 0) (toUId z)

-- | Random impulses.
--
--  Dust2 [KR,AR] density=0.0;    NONDET
dust2 :: ID a => a -> Rate -> UGen -> UGen
dust2 z rate density = mkUGen Nothing [KR,AR] (Left rate) "Dust2" [density] Nothing 1 (Special 0) (toUId z)

-- | Demand results from demand rate UGens.
--
--  Duty [KR,AR] dur=1.0 reset=0.0 doneAction=0.0 level=1.0;    REORDERS INPUTS: [0,1,3,2], ENUMERATION INPUTS: 2=DoneAction
duty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen
duty rate dur reset doneAction level = mkUGen Nothing [KR,AR] (Left rate) "Duty" [dur,reset,(from_done_action doneAction),level] Nothing 1 (Special 0) NoId

-- | Demand rate white noise random generator.
--
--  Dwhite [DR] length=1.0e8 lo=0.0 hi=1.0;    REORDERS INPUTS: [1,2,0], DEMAND/NONDET
dwhite :: ID a => a -> UGen -> UGen -> UGen -> UGen
dwhite z length_ lo hi = mkUGen Nothing [DR] (Left DR) "Dwhite" [length_,lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Demand rate random sequence generator.
--
--  Dxrand [DR] repeats=1.0 *list=0.0;    MCE, REORDERS INPUTS: [1,0], DEMAND/NONDET
dxrand :: ID a => a -> UGen -> UGen -> UGen
dxrand z repeats list_ = mkUGen Nothing [DR] (Left DR) "Dxrand" [repeats] (Just list_) 1 (Special 0) (toUId z)

-- | Envelope generator
--
--  EnvGen [KR,AR] gate=1.0 levelScale=1.0 levelBias=0.0 timeScale=1.0 doneAction=0.0 *envelope=0.0;    MCE, REORDERS INPUTS: [5,0,1,2,3,4,5], ENUMERATION INPUTS: 4=DoneAction, 5=Envelope UGen
envGen :: Rate -> UGen -> UGen -> UGen -> UGen -> DoneAction -> Envelope UGen -> UGen
envGen rate gate_ levelScale levelBias timeScale doneAction envelope_ = mkUGen Nothing [KR,AR] (Left rate) "EnvGen" [gate_,levelScale,levelBias,timeScale,(from_done_action doneAction)] (Just (envelope_to_ugen envelope_)) 1 (Special 0) NoId

-- | Exponential single random number generator.
--
--  ExpRand [IR] lo=1.0e-2 hi=1.0;    FILTER: TRUE, NONDET
expRand :: ID a => a -> UGen -> UGen -> UGen
expRand z lo hi = mkUGen Nothing [IR] (Left IR) "ExpRand" [lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Feedback sine with chaotic phase indexing
--
--  FBSineC [AR] freq=22050.0 im=1.0 fb=0.1 a=1.1 c=0.5 xi=0.1 yi=0.1
fbSineC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fbSineC rate freq im fb a c xi yi = mkUGen Nothing [AR] (Left rate) "FBSineC" [freq,im,fb,a,c,xi,yi] Nothing 1 (Special 0) NoId

-- | Feedback sine with chaotic phase indexing
--
--  FBSineL [AR] freq=22050.0 im=1.0 fb=0.1 a=1.1 c=0.5 xi=0.1 yi=0.1
fbSineL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fbSineL rate freq im fb a c xi yi = mkUGen Nothing [AR] (Left rate) "FBSineL" [freq,im,fb,a,c,xi,yi] Nothing 1 (Special 0) NoId

-- | Feedback sine with chaotic phase indexing
--
--  FBSineN [AR] freq=22050.0 im=1.0 fb=0.1 a=1.1 c=0.5 xi=0.1 yi=0.1
fbSineN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fbSineN rate freq im fb a c xi yi = mkUGen Nothing [AR] (Left rate) "FBSineN" [freq,im,fb,a,c,xi,yi] Nothing 1 (Special 0) NoId

-- | Fast Fourier Transform
--
--  FFT [KR] buffer=0.0 in=0.0 hop=0.5 wintype=0.0 active=1.0 winsize=0.0
fft :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fft buffer in_ hop wintype active winsize = mkUGen Nothing [KR] (Left KR) "FFT" [buffer,in_,hop,wintype,active,winsize] Nothing 1 (Special 0) NoId

-- | First order filter section.
--
--  FOS [KR,AR] in=0.0 a0=0.0 a1=0.0 b1=0.0;    FILTER: TRUE
fos :: UGen -> UGen -> UGen -> UGen -> UGen
fos in_ a0 a1 b1 = mkUGen Nothing [KR,AR] (Right [0]) "FOS" [in_,a0,a1,b1] Nothing 1 (Special 0) NoId

-- | Fast sine oscillator.
--
--  FSinOsc [KR,AR] freq=440.0 iphase=0.0
fSinOsc :: Rate -> UGen -> UGen -> UGen
fSinOsc rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "FSinOsc" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Fold a signal outside given thresholds.
--
--  Fold [IR,KR,AR] in=0.0 lo=0.0 hi=1.0;    FILTER: TRUE
fold :: UGen -> UGen -> UGen -> UGen
fold in_ lo hi = mkUGen Nothing [IR,KR,AR] (Right [0]) "Fold" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Formant oscillator
--
--  Formant [AR] fundfreq=440.0 formfreq=1760.0 bwfreq=880.0
formant :: Rate -> UGen -> UGen -> UGen -> UGen
formant rate fundfreq formfreq bwfreq = mkUGen Nothing [AR] (Left rate) "Formant" [fundfreq,formfreq,bwfreq] Nothing 1 (Special 0) NoId

-- | FOF-like filter.
--
--  Formlet [KR,AR] in=0.0 freq=440.0 attacktime=1.0 decaytime=1.0;    FILTER: TRUE
formlet :: UGen -> UGen -> UGen -> UGen -> UGen
formlet in_ freq attacktime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "Formlet" [in_,freq,attacktime,decaytime] Nothing 1 (Special 0) NoId

-- | When triggered, frees a node.
--
--  Free [KR] trig=0.0 id=0.0;    FILTER: TRUE
free :: UGen -> UGen -> UGen
free trig_ id_ = mkUGen Nothing [KR] (Right [0]) "Free" [trig_,id_] Nothing 1 (Special 0) NoId

-- | When triggered, free enclosing synth.
--
--  FreeSelf [KR] in=0.0
freeSelf :: UGen -> UGen
freeSelf in_ = mkUGen Nothing [KR] (Left KR) "FreeSelf" [in_] Nothing 1 (Special 0) NoId

-- | Free the enclosing synth when a UGen is finished
--
--  FreeSelfWhenDone [KR] src=0.0
freeSelfWhenDone :: UGen -> UGen
freeSelfWhenDone src = mkUGen Nothing [KR] (Left KR) "FreeSelfWhenDone" [src] Nothing 1 (Special 0) NoId

-- | A reverb
--
--  FreeVerb [AR] in=0.0 mix=0.33 room=0.5 damp=0.5;    FILTER: TRUE
freeVerb :: UGen -> UGen -> UGen -> UGen -> UGen
freeVerb in_ mix room damp = mkUGen Nothing [AR] (Right [0]) "FreeVerb" [in_,mix,room,damp] Nothing 1 (Special 0) NoId

-- | A two-channel reverb
--
--  FreeVerb2 [AR] in=0.0 in2=0.0 mix=0.33 room=0.5 damp=0.5;    FILTER: TRUE
freeVerb2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
freeVerb2 in_ in2 mix room damp = mkUGen Nothing [AR] (Right [0]) "FreeVerb2" [in_,in2,mix,room,damp] Nothing 2 (Special 0) NoId

-- | Frequency Shifter.
--
--  FreqShift [AR] in=0.0 freq=0.0 phase=0.0
freqShift :: UGen -> UGen -> UGen -> UGen
freqShift in_ freq phase = mkUGen Nothing [AR] (Left AR) "FreqShift" [in_,freq,phase] Nothing 1 (Special 0) NoId

-- | A two-channel reverb
--
--  GVerb [AR] in=0.0 roomsize=10.0 revtime=3.0 damping=0.5 inputbw=0.5 spread=15.0 drylevel=1.0 earlyreflevel=0.7 taillevel=0.5 maxroomsize=300.0;    FILTER: TRUE
gVerb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gVerb in_ roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize = mkUGen Nothing [AR] (Right [0]) "GVerb" [in_,roomsize,revtime,damping,inputbw,spread,drylevel,earlyreflevel,taillevel,maxroomsize] Nothing 2 (Special 0) NoId

-- | Gate or hold.
--
--  Gate [KR,AR] in=0.0 trig=0.0;    FILTER: TRUE
gate :: UGen -> UGen -> UGen
gate in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "Gate" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Gingerbreadman map chaotic generator
--
--  GbmanL [AR] freq=22050.0 xi=1.2 yi=2.1
gbmanL :: Rate -> UGen -> UGen -> UGen -> UGen
gbmanL rate freq xi yi = mkUGen Nothing [AR] (Left rate) "GbmanL" [freq,xi,yi] Nothing 1 (Special 0) NoId

-- | Gingerbreadman map chaotic generator
--
--  GbmanN [AR] freq=22050.0 xi=1.2 yi=2.1
gbmanN :: Rate -> UGen -> UGen -> UGen -> UGen
gbmanN rate freq xi yi = mkUGen Nothing [AR] (Left rate) "GbmanN" [freq,xi,yi] Nothing 1 (Special 0) NoId

-- | Dynamic stochastic synthesis generator.
--
--  Gendy1 [KR,AR] ampdist=1.0 durdist=1.0 adparam=1.0 ddparam=1.0 minfreq=440.0 maxfreq=660.0 ampscale=0.5 durscale=0.5 initCPs=12.0 knum=0.0;    NONDET
gendy1 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy1 z rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum = mkUGen Nothing [KR,AR] (Left rate) "Gendy1" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) (toUId z)

-- | Dynamic stochastic synthesis generator.
--
--  Gendy2 [KR,AR] ampdist=1.0 durdist=1.0 adparam=1.0 ddparam=1.0 minfreq=440.0 maxfreq=660.0 ampscale=0.5 durscale=0.5 initCPs=12.0 knum=0.0 a=1.17 c=0.31;    NONDET
gendy2 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy2 z rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c = mkUGen Nothing [KR,AR] (Left rate) "Gendy2" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum,a,c] Nothing 1 (Special 0) (toUId z)

-- | Dynamic stochastic synthesis generator.
--
--  Gendy3 [KR,AR] ampdist=1.0 durdist=1.0 adparam=1.0 ddparam=1.0 freq=440.0 ampscale=0.5 durscale=0.5 initCPs=12.0 knum=0.0;    NONDET
gendy3 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy3 z rate ampdist durdist adparam ddparam freq ampscale durscale initCPs knum = mkUGen Nothing [KR,AR] (Left rate) "Gendy3" [ampdist,durdist,adparam,ddparam,freq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) (toUId z)

-- | Granular synthesis with sound stored in a buffer
--
--  GrainBuf [AR] trigger=0.0 dur=1.0 sndbuf=0.0 rate=1.0 pos=0.0 interp=2.0 pan=0.0 envbufnum=-1.0 maxGrains=512.0;    NC INPUT: True
grainBuf :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainBuf numChannels trigger dur sndbuf rate_ pos interp pan envbufnum maxGrains = mkUGen Nothing [AR] (Left AR) "GrainBuf" [trigger,dur,sndbuf,rate_,pos,interp,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Granular synthesis with frequency modulated sine tones
--
--  GrainFM [AR] trigger=0.0 dur=1.0 carfreq=440.0 modfreq=200.0 index=1.0 pan=0.0 envbufnum=-1.0 maxGrains=512.0;    NC INPUT: True
grainFM :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainFM numChannels trigger dur carfreq modfreq index_ pan envbufnum maxGrains = mkUGen Nothing [AR] (Left AR) "GrainFM" [trigger,dur,carfreq,modfreq,index_,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Granulate an input signal
--
--  GrainIn [AR] trigger=0.0 dur=1.0 in=0.0 pan=0.0 envbufnum=-1.0 maxGrains=512.0;    NC INPUT: True
grainIn :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainIn numChannels trigger dur in_ pan envbufnum maxGrains = mkUGen Nothing [AR] (Left AR) "GrainIn" [trigger,dur,in_,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Granular synthesis with sine tones
--
--  GrainSin [AR] trigger=0.0 dur=1.0 freq=440.0 pan=0.0 envbufnum=-1.0 maxGrains=512.0;    NC INPUT: True
grainSin :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainSin numChannels trigger dur freq pan envbufnum maxGrains = mkUGen Nothing [AR] (Left AR) "GrainSin" [trigger,dur,freq,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Gray Noise.
--
--  GrayNoise [KR,AR] ;    NONDET
grayNoise :: ID a => a -> Rate -> UGen
grayNoise z rate = mkUGen Nothing [KR,AR] (Left rate) "GrayNoise" [] Nothing 1 (Special 0) (toUId z)

-- | 2nd order Butterworth highpass filter.
--
--  HPF [KR,AR] in=0.0 freq=440.0;    FILTER: TRUE
hpf :: UGen -> UGen -> UGen
hpf in_ freq = mkUGen Nothing [KR,AR] (Right [0]) "HPF" [in_,freq] Nothing 1 (Special 0) NoId

-- | Two point difference filter
--
--  HPZ1 [KR,AR] in=0.0;    FILTER: TRUE
hpz1 :: UGen -> UGen
hpz1 in_ = mkUGen Nothing [KR,AR] (Right [0]) "HPZ1" [in_] Nothing 1 (Special 0) NoId

-- | Two zero fixed midcut.
--
--  HPZ2 [KR,AR] in=0.0;    FILTER: TRUE
hPZ2 :: UGen -> UGen
hPZ2 in_ = mkUGen Nothing [KR,AR] (Right [0]) "HPZ2" [in_] Nothing 1 (Special 0) NoId

-- | Randomized value.
--
--  Hasher [KR,AR] in=0.0;    FILTER: TRUE
hasher :: UGen -> UGen
hasher in_ = mkUGen Nothing [KR,AR] (Right [0]) "Hasher" [in_] Nothing 1 (Special 0) NoId

-- | Henon map chaotic generator
--
--  HenonC [AR] freq=22050.0 a=1.4 b=0.3 x0=0.0 x1=0.0
henonC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonC rate freq a b x0 x1 = mkUGen Nothing [AR] (Left rate) "HenonC" [freq,a,b,x0,x1] Nothing 1 (Special 0) NoId

-- | Henon map chaotic generator
--
--  HenonL [AR] freq=22050.0 a=1.4 b=0.3 x0=0.0 x1=0.0
henonL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonL rate freq a b x0 x1 = mkUGen Nothing [AR] (Left rate) "HenonL" [freq,a,b,x0,x1] Nothing 1 (Special 0) NoId

-- | Henon map chaotic generator
--
--  HenonN [AR] freq=22050.0 a=1.4 b=0.3 x0=0.0 x1=0.0
henonN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonN rate freq a b x0 x1 = mkUGen Nothing [AR] (Left rate) "HenonN" [freq,a,b,x0,x1] Nothing 1 (Special 0) NoId

-- | Applies the Hilbert transform to an input signal.
--
--  Hilbert [AR] in=0.0;    FILTER: TRUE
hilbert :: UGen -> UGen
hilbert in_ = mkUGen Nothing [AR] (Right [0]) "Hilbert" [in_] Nothing 2 (Special 0) NoId

-- | Applies the Hilbert transform to an input signal.
--
--  HilbertFIR [AR] in=0.0 buffer=0.0
hilbertFIR :: Rate -> UGen -> UGen -> UGen
hilbertFIR rate in_ buffer = mkUGen Nothing [AR] (Left rate) "HilbertFIR" [in_,buffer] Nothing 2 (Special 0) NoId

-- | Envelope generator for polling values from an Env
--
--  IEnvGen [KR,AR] index=0.0 *envelope=0.0;    MCE, REORDERS INPUTS: [1,0], ENUMERATION INPUTS: 1=Envelope UGen
iEnvGen :: Rate -> UGen -> Envelope UGen -> UGen
iEnvGen rate index_ envelope_ = mkUGen Nothing [KR,AR] (Left rate) "IEnvGen" [index_] (Just (envelope_to_ugen envelope_)) 1 (Special 0) NoId

-- | Inverse Fast Fourier Transform
--
--  IFFT [KR,AR] buffer=0.0 wintype=0.0 winsize=0.0
ifft :: UGen -> UGen -> UGen -> UGen
ifft buffer wintype winsize = mkUGen Nothing [KR,AR] (Left AR) "IFFT" [buffer,wintype,winsize] Nothing 1 (Special 0) NoId

-- | Single integer random number generator.
--
--  IRand [IR] lo=0.0 hi=127.0;    NONDET
iRand :: ID a => a -> UGen -> UGen -> UGen
iRand z lo hi = mkUGen Nothing [IR] (Left IR) "IRand" [lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Impulse oscillator.
--
--  Impulse [KR,AR] freq=440.0 phase=0.0
impulse :: Rate -> UGen -> UGen -> UGen
impulse rate freq phase = mkUGen Nothing [KR,AR] (Left rate) "Impulse" [freq,phase] Nothing 1 (Special 0) NoId

-- | Read a signal from a bus.
--
--  In [KR,AR] bus=0.0;    NC INPUT: True
in' :: Int -> Rate -> UGen -> UGen
in' numChannels rate bus = mkUGen Nothing [KR,AR] (Left rate) "In" [bus] Nothing numChannels (Special 0) NoId

-- | Read signal from a bus with a current or one cycle old timestamp.
--
--  InFeedback [AR] bus=0.0;    NC INPUT: True
inFeedback :: Int -> UGen -> UGen
inFeedback numChannels bus = mkUGen Nothing [AR] (Left AR) "InFeedback" [bus] Nothing numChannels (Special 0) NoId

-- | Tests if a signal is within a given range.
--
--  InRange [IR,KR,AR] in=0.0 lo=0.0 hi=1.0;    FILTER: TRUE
inRange :: UGen -> UGen -> UGen -> UGen
inRange in_ lo hi = mkUGen Nothing [IR,KR,AR] (Right [0]) "InRange" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Test if a point is within a given rectangle.
--
--  InRect [KR,AR] x=0.0 y=0.0 rect=0.0
inRect :: Rate -> UGen -> UGen -> UGen -> UGen
inRect rate x y rect = mkUGen Nothing [KR,AR] (Left rate) "InRect" [x,y,rect] Nothing 1 (Special 0) NoId

-- | Generate a trigger anytime a bus is set.
--
--  InTrig [KR] bus=0.0;    NC INPUT: True
inTrig :: Int -> UGen -> UGen
inTrig numChannels bus = mkUGen Nothing [KR] (Left KR) "InTrig" [bus] Nothing numChannels (Special 0) NoId

-- | Index into a table with a signal
--
--  Index [KR,AR] bufnum=0.0 in=0.0;    FILTER: TRUE
index :: UGen -> UGen -> UGen
index bufnum in_ = mkUGen Nothing [KR,AR] (Right [1]) "Index" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Finds the (lowest) point in the Buffer at which the input signal lies in-between the two values
--
--  IndexInBetween [KR,AR] bufnum=0.0 in=0.0;    FILTER: TRUE
indexInBetween :: UGen -> UGen -> UGen
indexInBetween bufnum in_ = mkUGen Nothing [KR,AR] (Right [1]) "IndexInBetween" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Index into a table with a signal, linear interpolated
--
--  IndexL [KR,AR] bufnum=0.0 in=0.0
indexL :: Rate -> UGen -> UGen -> UGen
indexL rate bufnum in_ = mkUGen Nothing [KR,AR] (Left rate) "IndexL" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Base class for info ugens
--
--  InfoUGenBase [IR] 
infoUGenBase :: Rate -> UGen
infoUGenBase rate = mkUGen Nothing [IR] (Left rate) "InfoUGenBase" [] Nothing 1 (Special 0) NoId

-- | A leaky integrator.
--
--  Integrator [KR,AR] in=0.0 coef=1.0;    FILTER: TRUE
integrator :: UGen -> UGen -> UGen
integrator in_ coef = mkUGen Nothing [KR,AR] (Right [0]) "Integrator" [in_,coef] Nothing 1 (Special 0) NoId

-- | Control to audio rate converter.
--
--  K2A [AR] in=0.0
k2A :: UGen -> UGen
k2A in_ = mkUGen Nothing [AR] (Left AR) "K2A" [in_] Nothing 1 (Special 0) NoId

-- | Respond to the state of a key
--
--  KeyState [KR] keycode=0.0 minval=0.0 maxval=1.0 lag=0.2
keyState :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
keyState rate keycode minval maxval lag_ = mkUGen Nothing [KR] (Left rate) "KeyState" [keycode,minval,maxval,lag_] Nothing 1 (Special 0) NoId

-- | Key tracker
--
--  KeyTrack [KR] chain=0.0 keydecay=2.0 chromaleak=0.5
keyTrack :: Rate -> UGen -> UGen -> UGen -> UGen
keyTrack rate chain keydecay chromaleak = mkUGen Nothing [KR] (Left rate) "KeyTrack" [chain,keydecay,chromaleak] Nothing 1 (Special 0) NoId

-- | Sine oscillator bank
--
--  Klang [AR] freqscale=1.0 freqoffset=0.0 *specificationsArrayRef=0.0;    MCE, REORDERS INPUTS: [2,0,1]
klang :: Rate -> UGen -> UGen -> UGen -> UGen
klang rate freqscale freqoffset specificationsArrayRef = mkUGen Nothing [AR] (Left rate) "Klang" [freqscale,freqoffset] (Just specificationsArrayRef) 1 (Special 0) NoId

-- | Bank of resonators
--
--  Klank [AR] input=0.0 freqscale=1.0 freqoffset=0.0 decayscale=1.0 *specificationsArrayRef=0.0;    MCE, FILTER: TRUE, REORDERS INPUTS: [4,0,1,2,3]
klank :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
klank input freqscale freqoffset decayscale specificationsArrayRef = mkUGen Nothing [AR] (Right [0]) "Klank" [input,freqscale,freqoffset,decayscale] (Just specificationsArrayRef) 1 (Special 0) NoId

-- | Clipped noise
--
--  LFClipNoise [KR,AR] freq=500.0;    NONDET
lfClipNoise :: ID a => a -> Rate -> UGen -> UGen
lfClipNoise z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFClipNoise" [freq] Nothing 1 (Special 0) (toUId z)

-- | A sine like shape made of two cubic pieces
--
--  LFCub [KR,AR] freq=440.0 iphase=0.0
lfCub :: Rate -> UGen -> UGen -> UGen
lfCub rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "LFCub" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Dynamic clipped noise
--
--  LFDClipNoise [KR,AR] freq=500.0;    NONDET
lfdClipNoise :: ID a => a -> Rate -> UGen -> UGen
lfdClipNoise z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFDClipNoise" [freq] Nothing 1 (Special 0) (toUId z)

-- | Dynamic step noise
--
--  LFDNoise0 [KR,AR] freq=500.0;    NONDET
lfdNoise0 :: ID a => a -> Rate -> UGen -> UGen
lfdNoise0 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFDNoise0" [freq] Nothing 1 (Special 0) (toUId z)

-- | Dynamic ramp noise
--
--  LFDNoise1 [KR,AR] freq=500.0;    NONDET
lfdNoise1 :: ID a => a -> Rate -> UGen -> UGen
lfdNoise1 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFDNoise1" [freq] Nothing 1 (Special 0) (toUId z)

-- | Dynamic cubic noise
--
--  LFDNoise3 [KR,AR] freq=500.0;    NONDET
lfdNoise3 :: ID a => a -> Rate -> UGen -> UGen
lfdNoise3 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFDNoise3" [freq] Nothing 1 (Special 0) (toUId z)

-- | Gaussian function oscillator
--
--  LFGauss [KR,AR] duration=1.0 width=0.1 iphase=0.0 loop=1.0 doneAction=0.0;    ENUMERATION INPUTS: 3=Loop, 4=DoneAction
lfGauss :: Rate -> UGen -> UGen -> UGen -> Loop -> DoneAction -> UGen
lfGauss rate duration width iphase loop doneAction = mkUGen Nothing [KR,AR] (Left rate) "LFGauss" [duration,width,iphase,(from_loop loop),(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Step noise
--
--  LFNoise0 [KR,AR] freq=500.0;    NONDET
lfNoise0 :: ID a => a -> Rate -> UGen -> UGen
lfNoise0 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFNoise0" [freq] Nothing 1 (Special 0) (toUId z)

-- | Ramp noise
--
--  LFNoise1 [KR,AR] freq=500.0;    NONDET
lfNoise1 :: ID a => a -> Rate -> UGen -> UGen
lfNoise1 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFNoise1" [freq] Nothing 1 (Special 0) (toUId z)

-- | Quadratic noise.
--
--  LFNoise2 [KR,AR] freq=500.0;    NONDET
lfNoise2 :: ID a => a -> Rate -> UGen -> UGen
lfNoise2 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFNoise2" [freq] Nothing 1 (Special 0) (toUId z)

-- | Parabolic oscillator
--
--  LFPar [KR,AR] freq=440.0 iphase=0.0
lfPar :: Rate -> UGen -> UGen -> UGen
lfPar rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "LFPar" [freq,iphase] Nothing 1 (Special 0) NoId

-- | pulse oscillator
--
--  LFPulse [KR,AR] freq=440.0 iphase=0.0 width=0.5
lfPulse :: Rate -> UGen -> UGen -> UGen -> UGen
lfPulse rate freq iphase width = mkUGen Nothing [KR,AR] (Left rate) "LFPulse" [freq,iphase,width] Nothing 1 (Special 0) NoId

-- | Sawtooth oscillator
--
--  LFSaw [KR,AR] freq=440.0 iphase=0.0
lfSaw :: Rate -> UGen -> UGen -> UGen
lfSaw rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "LFSaw" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Triangle oscillator
--
--  LFTri [KR,AR] freq=440.0 iphase=0.0
lfTri :: Rate -> UGen -> UGen -> UGen
lfTri rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "LFTri" [freq,iphase] Nothing 1 (Special 0) NoId

-- | 2nd order Butterworth lowpass filter
--
--  LPF [KR,AR] in=0.0 freq=440.0;    FILTER: TRUE
lpf :: UGen -> UGen -> UGen
lpf in_ freq = mkUGen Nothing [KR,AR] (Right [0]) "LPF" [in_,freq] Nothing 1 (Special 0) NoId

-- | Two point average filter
--
--  LPZ1 [KR,AR] in=0.0;    FILTER: TRUE
lpz1 :: UGen -> UGen
lpz1 in_ = mkUGen Nothing [KR,AR] (Right [0]) "LPZ1" [in_] Nothing 1 (Special 0) NoId

-- | Two zero fixed lowpass
--
--  LPZ2 [KR,AR] in=0.0;    FILTER: TRUE
lPZ2 :: UGen -> UGen
lPZ2 in_ = mkUGen Nothing [KR,AR] (Right [0]) "LPZ2" [in_] Nothing 1 (Special 0) NoId

-- | Exponential lag
--
--  Lag [KR,AR] in=0.0 lagTime=0.1;    FILTER: TRUE
lag :: UGen -> UGen -> UGen
lag in_ lagTime = mkUGen Nothing [KR,AR] (Right [0]) "Lag" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Exponential lag
--
--  Lag2 [KR,AR] in=0.0 lagTime=0.1;    FILTER: TRUE
lag2 :: UGen -> UGen -> UGen
lag2 in_ lagTime = mkUGen Nothing [KR,AR] (Right [0]) "Lag2" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Exponential lag
--
--  Lag2UD [KR,AR] in=0.0 lagTimeU=0.1 lagTimeD=0.1;    FILTER: TRUE
lag2UD :: UGen -> UGen -> UGen -> UGen
lag2UD in_ lagTimeU lagTimeD = mkUGen Nothing [KR,AR] (Right [0]) "Lag2UD" [in_,lagTimeU,lagTimeD] Nothing 1 (Special 0) NoId

-- | Exponential lag
--
--  Lag3 [KR,AR] in=0.0 lagTime=0.1;    FILTER: TRUE
lag3 :: UGen -> UGen -> UGen
lag3 in_ lagTime = mkUGen Nothing [KR,AR] (Right [0]) "Lag3" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Exponential lag
--
--  Lag3UD [KR,AR] in=0.0 lagTimeU=0.1 lagTimeD=0.1;    FILTER: TRUE
lag3UD :: UGen -> UGen -> UGen -> UGen
lag3UD in_ lagTimeU lagTimeD = mkUGen Nothing [KR,AR] (Right [0]) "Lag3UD" [in_,lagTimeU,lagTimeD] Nothing 1 (Special 0) NoId

-- | Read a control signal from a bus with a lag
--
--  LagIn [KR] bus=0.0 lag=0.1;    NC INPUT: True
lagIn :: Int -> UGen -> UGen -> UGen
lagIn numChannels bus lag_ = mkUGen Nothing [KR] (Left KR) "LagIn" [bus,lag_] Nothing numChannels (Special 0) NoId

-- | Exponential lag
--
--  LagUD [KR,AR] in=0.0 lagTimeU=0.1 lagTimeD=0.1;    FILTER: TRUE
lagUD :: UGen -> UGen -> UGen -> UGen
lagUD in_ lagTimeU lagTimeD = mkUGen Nothing [KR,AR] (Right [0]) "LagUD" [in_,lagTimeU,lagTimeD] Nothing 1 (Special 0) NoId

-- | Output the last value before the input changed
--
--  LastValue [KR,AR] in=0.0 diff=1.0e-2;    FILTER: TRUE
lastValue :: UGen -> UGen -> UGen
lastValue in_ diff = mkUGen Nothing [KR,AR] (Right [0]) "LastValue" [in_,diff] Nothing 1 (Special 0) NoId

-- | Sample and hold
--
--  Latch [KR,AR] in=0.0 trig=0.0;    FILTER: TRUE
latch :: UGen -> UGen -> UGen
latch in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "Latch" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Latoocarfian chaotic generator
--
--  LatoocarfianC [AR] freq=22050.0 a=1.0 b=3.0 c=0.5 d=0.5 xi=0.5 yi=0.5
latoocarfianC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianC rate freq a b c d xi yi = mkUGen Nothing [AR] (Left rate) "LatoocarfianC" [freq,a,b,c,d,xi,yi] Nothing 1 (Special 0) NoId

-- | Latoocarfian chaotic generator
--
--  LatoocarfianL [AR] freq=22050.0 a=1.0 b=3.0 c=0.5 d=0.5 xi=0.5 yi=0.5
latoocarfianL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianL rate freq a b c d xi yi = mkUGen Nothing [AR] (Left rate) "LatoocarfianL" [freq,a,b,c,d,xi,yi] Nothing 1 (Special 0) NoId

-- | Latoocarfian chaotic generator
--
--  LatoocarfianN [AR] freq=22050.0 a=1.0 b=3.0 c=0.5 d=0.5 xi=0.5 yi=0.5
latoocarfianN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianN rate freq a b c d xi yi = mkUGen Nothing [AR] (Left rate) "LatoocarfianN" [freq,a,b,c,d,xi,yi] Nothing 1 (Special 0) NoId

-- | Remove DC
--
--  LeakDC [KR,AR] in=0.0 coef=0.995;    FILTER: TRUE
leakDC :: UGen -> UGen -> UGen
leakDC in_ coef = mkUGen Nothing [KR,AR] (Right [0]) "LeakDC" [in_,coef] Nothing 1 (Special 0) NoId

-- | Output least changed
--
--  LeastChange [KR,AR] a=0.0 b=0.0
leastChange :: Rate -> UGen -> UGen -> UGen
leastChange rate a b = mkUGen Nothing [KR,AR] (Left rate) "LeastChange" [a,b] Nothing 1 (Special 0) NoId

-- | Peak limiter
--
--  Limiter [AR] in=0.0 level=1.0 dur=1.0e-2;    FILTER: TRUE
limiter :: UGen -> UGen -> UGen -> UGen
limiter in_ level dur = mkUGen Nothing [AR] (Right [0]) "Limiter" [in_,level,dur] Nothing 1 (Special 0) NoId

-- | Linear congruential chaotic generator
--
--  LinCongC [AR] freq=22050.0 a=1.1 c=0.13 m=1.0 xi=0.0
linCongC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongC rate freq a c m xi = mkUGen Nothing [AR] (Left rate) "LinCongC" [freq,a,c,m,xi] Nothing 1 (Special 0) NoId

-- | Linear congruential chaotic generator
--
--  LinCongL [AR] freq=22050.0 a=1.1 c=0.13 m=1.0 xi=0.0
linCongL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongL rate freq a c m xi = mkUGen Nothing [AR] (Left rate) "LinCongL" [freq,a,c,m,xi] Nothing 1 (Special 0) NoId

-- | Linear congruential chaotic generator
--
--  LinCongN [AR] freq=22050.0 a=1.1 c=0.13 m=1.0 xi=0.0
linCongN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongN rate freq a c m xi = mkUGen Nothing [AR] (Left rate) "LinCongN" [freq,a,c,m,xi] Nothing 1 (Special 0) NoId

-- | Map a linear range to an exponential range
--
--  LinExp [KR,AR] in=0.0 srclo=0.0 srchi=1.0 dstlo=1.0 dsthi=2.0;    FILTER: TRUE
linExp :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linExp in_ srclo srchi dstlo dsthi = mkUGen Nothing [KR,AR] (Right [0]) "LinExp" [in_,srclo,srchi,dstlo,dsthi] Nothing 1 (Special 0) NoId

-- | Two channel linear pan.
--
--  LinPan2 [KR,AR] in=0.0 pos=0.0 level=1.0;    FILTER: TRUE
linPan2 :: UGen -> UGen -> UGen -> UGen
linPan2 in_ pos level = mkUGen Nothing [KR,AR] (Right [0]) "LinPan2" [in_,pos,level] Nothing 2 (Special 0) NoId

-- | Skewed random number generator.
--
--  LinRand [IR] lo=0.0 hi=1.0 minmax=0.0;    NONDET
linRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
linRand z lo hi minmax = mkUGen Nothing [IR] (Left IR) "LinRand" [lo,hi,minmax] Nothing 1 (Special 0) (toUId z)

-- | Two channel linear crossfade.
--
--  LinXFade2 [KR,AR] inA=0.0 inB=0.0 pan=0.0 level=1.0;    FILTER: TRUE, PSUEDO INPUTS: [3]
linXFade2 :: UGen -> UGen -> UGen -> UGen -> UGen
linXFade2 inA inB pan level = mkUGen Nothing [KR,AR] (Right [0,1]) "LinXFade2" [inA,inB,pan,level] Nothing 1 (Special 0) NoId

-- | Line generator.
--
--  Line [KR,AR] start=0.0 end=1.0 dur=1.0 doneAction=0.0;    ENUMERATION INPUTS: 3=DoneAction
line :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
line rate start end dur doneAction = mkUGen Nothing [KR,AR] (Left rate) "Line" [start,end,dur,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Simple linear envelope generator.
--
--  Linen [KR] gate=1.0 attackTime=1.0e-2 susLevel=1.0 releaseTime=1.0 doneAction=0.0;    ENUMERATION INPUTS: 4=DoneAction
linen :: UGen -> UGen -> UGen -> UGen -> DoneAction -> UGen
linen gate_ attackTime susLevel releaseTime doneAction = mkUGen Nothing [KR] (Left KR) "Linen" [gate_,attackTime,susLevel,releaseTime,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Allocate a buffer local to the synth
--
--  LocalBuf [IR] numChannels=1.0 numFrames=1.0;    REORDERS INPUTS: [1,0], NONDET
localBuf :: ID a => a -> UGen -> UGen -> UGen
localBuf z numChannels numFrames = mkUGen Nothing [IR] (Left IR) "LocalBuf" [numChannels,numFrames] Nothing 1 (Special 0) (toUId z)

-- | Define and read from buses local to a synth.
--
--  LocalIn [KR,AR] *default=0.0;    MCE, NC INPUT: True
localIn :: Int -> Rate -> UGen -> UGen
localIn numChannels rate default_ = mkUGen Nothing [KR,AR] (Left rate) "LocalIn" [] (Just default_) numChannels (Special 0) NoId

-- | Write to buses local to a synth.
--
--  LocalOut [KR,AR] *channelsArray=0.0;    MCE, FILTER: TRUE
localOut :: UGen -> UGen
localOut input = mkUGen Nothing [KR,AR] (Right [0]) "LocalOut" [] (Just input) 0 (Special 0) NoId

-- | Chaotic noise function
--
--  Logistic [KR,AR] chaosParam=3.0 freq=1000.0 init=0.5
logistic :: Rate -> UGen -> UGen -> UGen -> UGen
logistic rate chaosParam freq init_ = mkUGen Nothing [KR,AR] (Left rate) "Logistic" [chaosParam,freq,init_] Nothing 1 (Special 0) NoId

-- | Lorenz chaotic generator
--
--  LorenzL [AR] freq=22050.0 s=10.0 r=28.0 b=2.667 h=5.0e-2 xi=0.1 yi=0.0 zi=0.0
lorenzL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenzL rate freq s r b h xi yi zi = mkUGen Nothing [AR] (Left rate) "LorenzL" [freq,s,r,b,h,xi,yi,zi] Nothing 1 (Special 0) NoId

-- | Extraction of instantaneous loudness in sones
--
--  Loudness [KR] chain=0.0 smask=0.25 tmask=1.0
loudness :: UGen -> UGen -> UGen -> UGen
loudness chain smask tmask = mkUGen Nothing [KR] (Left KR) "Loudness" [chain,smask,tmask] Nothing 1 (Special 0) NoId

-- | Mel frequency cepstral coefficients
--
--  MFCC [KR] chain=0.0 numcoeff=13.0
mFCC :: Rate -> UGen -> UGen -> UGen
mFCC rate chain numcoeff = mkUGen Nothing [KR] (Left rate) "MFCC" [chain,numcoeff] Nothing 13 (Special 0) NoId

-- | Reduce precision.
--
--  MantissaMask [KR,AR] in=0.0 bits=3.0;    FILTER: TRUE
mantissaMask :: UGen -> UGen -> UGen
mantissaMask in_ bits = mkUGen Nothing [KR,AR] (Right [0]) "MantissaMask" [in_,bits] Nothing 1 (Special 0) NoId

-- | Median filter.
--
--  Median [KR,AR] length=3.0 in=0.0;    FILTER: TRUE
median :: UGen -> UGen -> UGen
median length_ in_ = mkUGen Nothing [KR,AR] (Right [1]) "Median" [length_,in_] Nothing 1 (Special 0) NoId

-- | Parametric filter.
--
--  MidEQ [KR,AR] in=0.0 freq=440.0 rq=1.0 db=0.0;    FILTER: TRUE
midEQ :: UGen -> UGen -> UGen -> UGen -> UGen
midEQ in_ freq rq db = mkUGen Nothing [KR,AR] (Right [0]) "MidEQ" [in_,freq,rq,db] Nothing 1 (Special 0) NoId

-- | Minimum difference of two values in modulo arithmetics
--
--  ModDif [IR,KR,AR] x=0.0 y=0.0 mod=1.0
modDif :: Rate -> UGen -> UGen -> UGen -> UGen
modDif rate x y mod_ = mkUGen Nothing [IR,KR,AR] (Left rate) "ModDif" [x,y,mod_] Nothing 1 (Special 0) NoId

-- | Moog VCF implementation, designed by Federico Fontana
--
--  MoogFF [KR,AR] in=0.0 freq=100.0 gain=2.0 reset=0.0;    FILTER: TRUE
moogFF :: UGen -> UGen -> UGen -> UGen -> UGen
moogFF in_ freq gain reset = mkUGen Nothing [KR,AR] (Right [0]) "MoogFF" [in_,freq,gain,reset] Nothing 1 (Special 0) NoId

-- | Output most changed.
--
--  MostChange [KR,AR] a=0.0 b=0.0;    FILTER: TRUE
mostChange :: UGen -> UGen -> UGen
mostChange a b = mkUGen Nothing [KR,AR] (Right [0,1]) "MostChange" [a,b] Nothing 1 (Special 0) NoId

-- | Mouse button UGen.
--
--  MouseButton [KR] minval=0.0 maxval=1.0 lag=0.2
mouseButton :: Rate -> UGen -> UGen -> UGen -> UGen
mouseButton rate minval maxval lag_ = mkUGen Nothing [KR] (Left rate) "MouseButton" [minval,maxval,lag_] Nothing 1 (Special 0) NoId

-- | Cursor tracking UGen.
--
--  MouseX [KR] minval=0.0 maxval=1.0 warp=0.0 lag=0.2;    ENUMERATION INPUTS: 2=Warp
mouseX :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseX rate minval maxval warp lag_ = mkUGen Nothing [KR] (Left rate) "MouseX" [minval,maxval,(from_warp warp),lag_] Nothing 1 (Special 0) NoId

-- | Cursor tracking UGen.
--
--  MouseY [KR] minval=0.0 maxval=1.0 warp=0.0 lag=0.2;    ENUMERATION INPUTS: 2=Warp
mouseY :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseY rate minval maxval warp lag_ = mkUGen Nothing [KR] (Left rate) "MouseY" [minval,maxval,(from_warp warp),lag_] Nothing 1 (Special 0) NoId

-- | Sum of uniform distributions.
--
--  NRand [IR] lo=0.0 hi=1.0 n=0.0;    NONDET
nRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
nRand z lo hi n = mkUGen Nothing [IR] (Left IR) "NRand" [lo,hi,n] Nothing 1 (Special 0) (toUId z)

-- | Flattens dynamics.
--
--  Normalizer [AR] in=0.0 level=1.0 dur=1.0e-2;    FILTER: TRUE
normalizer :: UGen -> UGen -> UGen -> UGen
normalizer in_ level dur = mkUGen Nothing [AR] (Right [0]) "Normalizer" [in_,level,dur] Nothing 1 (Special 0) NoId

-- | Number of audio busses.
--
--  NumAudioBuses [IR] 
numAudioBuses :: UGen
numAudioBuses = mkUGen Nothing [IR] (Left IR) "NumAudioBuses" [] Nothing 1 (Special 0) NoId

-- | Number of open buffers.
--
--  NumBuffers [IR] 
numBuffers :: UGen
numBuffers = mkUGen Nothing [IR] (Left IR) "NumBuffers" [] Nothing 1 (Special 0) NoId

-- | Number of control busses.
--
--  NumControlBuses [IR] 
numControlBuses :: UGen
numControlBuses = mkUGen Nothing [IR] (Left IR) "NumControlBuses" [] Nothing 1 (Special 0) NoId

-- | Number of input busses.
--
--  NumInputBuses [IR] 
numInputBuses :: UGen
numInputBuses = mkUGen Nothing [IR] (Left IR) "NumInputBuses" [] Nothing 1 (Special 0) NoId

-- | Number of output busses.
--
--  NumOutputBuses [IR] 
numOutputBuses :: UGen
numOutputBuses = mkUGen Nothing [IR] (Left IR) "NumOutputBuses" [] Nothing 1 (Special 0) NoId

-- | Number of currently running synths.
--
--  NumRunningSynths [IR,KR] 
numRunningSynths :: UGen
numRunningSynths = mkUGen Nothing [IR,KR] (Left IR) "NumRunningSynths" [] Nothing 1 (Special 0) NoId

-- | Write a signal to a bus with sample accurate timing.
--
--  OffsetOut [KR,AR] bus=0.0 *channelsArray=0.0;    MCE, FILTER: TRUE
offsetOut :: UGen -> UGen -> UGen
offsetOut bus input = mkUGen Nothing [KR,AR] (Right [1]) "OffsetOut" [bus] (Just input) 0 (Special 0) NoId

-- | One pole filter.
--
--  OnePole [KR,AR] in=0.0 coef=0.5;    FILTER: TRUE
onePole :: UGen -> UGen -> UGen
onePole in_ coef = mkUGen Nothing [KR,AR] (Right [0]) "OnePole" [in_,coef] Nothing 1 (Special 0) NoId

-- | One zero filter.
--
--  OneZero [KR,AR] in=0.0 coef=0.5;    FILTER: TRUE
oneZero :: UGen -> UGen -> UGen
oneZero in_ coef = mkUGen Nothing [KR,AR] (Right [0]) "OneZero" [in_,coef] Nothing 1 (Special 0) NoId

-- | Onset detector
--
--  Onsets [KR] chain=0.0 threshold=0.5 odftype=3.0 relaxtime=1.0 floor=0.1 mingap=10.0 medianspan=11.0 whtype=1.0 rawodf=0.0
onsets :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
onsets chain threshold odftype relaxtime floor_ mingap medianspan whtype rawodf = mkUGen Nothing [KR] (Left KR) "Onsets" [chain,threshold,odftype,relaxtime,floor_,mingap,medianspan,whtype,rawodf] Nothing 1 (Special 0) NoId

-- | Interpolating wavetable oscillator.
--
--  Osc [KR,AR] bufnum=0.0 freq=440.0 phase=0.0
osc :: Rate -> UGen -> UGen -> UGen -> UGen
osc rate bufnum freq phase = mkUGen Nothing [KR,AR] (Left rate) "Osc" [bufnum,freq,phase] Nothing 1 (Special 0) NoId

-- | Noninterpolating wavetable oscillator.
--
--  OscN [KR,AR] bufnum=0.0 freq=440.0 phase=0.0
oscN :: Rate -> UGen -> UGen -> UGen -> UGen
oscN rate bufnum freq phase = mkUGen Nothing [KR,AR] (Left rate) "OscN" [bufnum,freq,phase] Nothing 1 (Special 0) NoId

-- | Write a signal to a bus.
--
--  Out [KR,AR] bus=0.0 *channelsArray=0.0;    MCE, FILTER: TRUE
out :: UGen -> UGen -> UGen
out bus input = mkUGen Nothing [KR,AR] (Right [1]) "Out" [bus] (Just input) 0 (Special 0) NoId

-- | Very fast sine grain with a parabolic envelope
--
--  PSinGrain [AR] freq=440.0 dur=0.2 amp=1.0
pSinGrain :: Rate -> UGen -> UGen -> UGen -> UGen
pSinGrain rate freq dur amp = mkUGen Nothing [AR] (Left rate) "PSinGrain" [freq,dur,amp] Nothing 1 (Special 0) NoId

-- | Complex addition.
--
--  PV_Add [KR] bufferA=0.0 bufferB=0.0
pv_Add :: UGen -> UGen -> UGen
pv_Add bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Add" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Scramble bins.
--
--  PV_BinScramble [KR] buffer=0.0 wipe=0.0 width=0.2 trig=0.0;    NONDET
pv_BinScramble :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble z buffer wipe width trig_ = mkUGen Nothing [KR] (Left KR) "PV_BinScramble" [buffer,wipe,width,trig_] Nothing 1 (Special 0) (toUId z)

-- | Shift and stretch bin position.
--
--  PV_BinShift [KR] buffer=0.0 stretch=1.0 shift=0.0 interp=0.0
pv_BinShift :: UGen -> UGen -> UGen -> UGen -> UGen
pv_BinShift buffer stretch shift interp = mkUGen Nothing [KR] (Left KR) "PV_BinShift" [buffer,stretch,shift,interp] Nothing 1 (Special 0) NoId

-- | Combine low and high bins from two inputs.
--
--  PV_BinWipe [KR] bufferA=0.0 bufferB=0.0 wipe=0.0
pv_BinWipe :: UGen -> UGen -> UGen -> UGen
pv_BinWipe bufferA bufferB wipe = mkUGen Nothing [KR] (Left KR) "PV_BinWipe" [bufferA,bufferB,wipe] Nothing 1 (Special 0) NoId

-- | Zero bins.
--
--  PV_BrickWall [KR] buffer=0.0 wipe=0.0
pv_BrickWall :: UGen -> UGen -> UGen
pv_BrickWall buffer wipe = mkUGen Nothing [KR] (Left KR) "PV_BrickWall" [buffer,wipe] Nothing 1 (Special 0) NoId

-- | Base class for UGens that alter FFT chains
--
--  PV_ChainUGen [KR] maxSize=0.0
pv_ChainUGen :: UGen -> UGen
pv_ChainUGen maxSize = mkUGen Nothing [KR] (Left KR) "PV_ChainUGen" [maxSize] Nothing 1 (Special 0) NoId

-- | Complex plane attack.
--
--  PV_ConformalMap [KR] buffer=0.0 areal=0.0 aimag=0.0
pv_ConformalMap :: UGen -> UGen -> UGen -> UGen
pv_ConformalMap buffer areal aimag = mkUGen Nothing [KR] (Left KR) "PV_ConformalMap" [buffer,areal,aimag] Nothing 1 (Special 0) NoId

-- | Complex conjugate
--
--  PV_Conj [KR] buffer=0.0
pv_Conj :: UGen -> UGen
pv_Conj buffer = mkUGen Nothing [KR] (Left KR) "PV_Conj" [buffer] Nothing 1 (Special 0) NoId

-- | Copy an FFT buffer
--
--  PV_Copy [KR] bufferA=0.0 bufferB=0.0
pv_Copy :: UGen -> UGen -> UGen
pv_Copy bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Copy" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Copy magnitudes and phases.
--
--  PV_CopyPhase [KR] bufferA=0.0 bufferB=0.0
pv_CopyPhase :: UGen -> UGen -> UGen
pv_CopyPhase bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_CopyPhase" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Random phase shifting.
--
--  PV_Diffuser [KR] buffer=0.0 trig=0.0
pv_Diffuser :: UGen -> UGen -> UGen
pv_Diffuser buffer trig_ = mkUGen Nothing [KR] (Left KR) "PV_Diffuser" [buffer,trig_] Nothing 1 (Special 0) NoId

-- | Complex division
--
--  PV_Div [KR] bufferA=0.0 bufferB=0.0
pv_Div :: UGen -> UGen -> UGen
pv_Div bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Div" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | FFT onset detector.
--
--  PV_HainsworthFoote [KR,AR] maxSize=0.0
pv_HainsworthFoote :: UGen -> UGen
pv_HainsworthFoote maxSize = mkUGen Nothing [KR,AR] (Left KR) "PV_HainsworthFoote" [maxSize] Nothing 1 (Special 0) NoId

-- | FFT feature detector for onset detection.
--
--  PV_JensenAndersen [KR,AR] maxSize=0.0
pv_JensenAndersen :: UGen -> UGen
pv_JensenAndersen maxSize = mkUGen Nothing [KR,AR] (Left KR) "PV_JensenAndersen" [maxSize] Nothing 1 (Special 0) NoId

-- | Pass bins which are a local maximum.
--
--  PV_LocalMax [KR] buffer=0.0 threshold=0.0
pv_LocalMax :: UGen -> UGen -> UGen
pv_LocalMax buffer threshold = mkUGen Nothing [KR] (Left KR) "PV_LocalMax" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Pass bins above a threshold.
--
--  PV_MagAbove [KR] buffer=0.0 threshold=0.0
pv_MagAbove :: UGen -> UGen -> UGen
pv_MagAbove buffer threshold = mkUGen Nothing [KR] (Left KR) "PV_MagAbove" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Pass bins below a threshold.
--
--  PV_MagBelow [KR] buffer=0.0 threshold=0.0
pv_MagBelow :: UGen -> UGen -> UGen
pv_MagBelow buffer threshold = mkUGen Nothing [KR] (Left KR) "PV_MagBelow" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Clip bins to a threshold.
--
--  PV_MagClip [KR] buffer=0.0 threshold=0.0
pv_MagClip :: UGen -> UGen -> UGen
pv_MagClip buffer threshold = mkUGen Nothing [KR] (Left KR) "PV_MagClip" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Division of magnitudes
--
--  PV_MagDiv [KR] bufferA=0.0 bufferB=0.0 zeroed=1.0e-4
pv_MagDiv :: UGen -> UGen -> UGen -> UGen
pv_MagDiv bufferA bufferB zeroed = mkUGen Nothing [KR] (Left KR) "PV_MagDiv" [bufferA,bufferB,zeroed] Nothing 1 (Special 0) NoId

-- | Freeze magnitudes.
--
--  PV_MagFreeze [KR] buffer=0.0 freeze=0.0
pv_MagFreeze :: UGen -> UGen -> UGen
pv_MagFreeze buffer freeze = mkUGen Nothing [KR] (Left KR) "PV_MagFreeze" [buffer,freeze] Nothing 1 (Special 0) NoId

-- | Multiply magnitudes.
--
--  PV_MagMul [KR] bufferA=0.0 bufferB=0.0
pv_MagMul :: UGen -> UGen -> UGen
pv_MagMul bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_MagMul" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Multiply magnitudes by noise.
--
--  PV_MagNoise [KR] buffer=0.0
pv_MagNoise :: UGen -> UGen
pv_MagNoise buffer = mkUGen Nothing [KR] (Left KR) "PV_MagNoise" [buffer] Nothing 1 (Special 0) NoId

-- | shift and stretch magnitude bin position.
--
--  PV_MagShift [KR] buffer=0.0 stretch=1.0 shift=0.0
pv_MagShift :: UGen -> UGen -> UGen -> UGen
pv_MagShift buffer stretch shift = mkUGen Nothing [KR] (Left KR) "PV_MagShift" [buffer,stretch,shift] Nothing 1 (Special 0) NoId

-- | Average magnitudes across bins.
--
--  PV_MagSmear [KR] buffer=0.0 bins=0.0
pv_MagSmear :: UGen -> UGen -> UGen
pv_MagSmear buffer bins = mkUGen Nothing [KR] (Left KR) "PV_MagSmear" [buffer,bins] Nothing 1 (Special 0) NoId

-- | Square magnitudes.
--
--  PV_MagSquared [KR] buffer=0.0
pv_MagSquared :: UGen -> UGen
pv_MagSquared buffer = mkUGen Nothing [KR] (Left KR) "PV_MagSquared" [buffer] Nothing 1 (Special 0) NoId

-- | Maximum magnitude.
--
--  PV_Max [KR] bufferA=0.0 bufferB=0.0
pv_Max :: UGen -> UGen -> UGen
pv_Max bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Max" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Minimum magnitude.
--
--  PV_Min [KR] bufferA=0.0 bufferB=0.0
pv_Min :: UGen -> UGen -> UGen
pv_Min bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Min" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Complex multiply.
--
--  PV_Mul [KR] bufferA=0.0 bufferB=0.0
pv_Mul :: UGen -> UGen -> UGen
pv_Mul bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Mul" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Shift phase.
--
--  PV_PhaseShift [KR] buffer=0.0 shift=0.0 integrate=0.0
pv_PhaseShift :: UGen -> UGen -> UGen -> UGen
pv_PhaseShift buffer shift integrate = mkUGen Nothing [KR] (Left KR) "PV_PhaseShift" [buffer,shift,integrate] Nothing 1 (Special 0) NoId

-- | Shift phase by 270 degrees.
--
--  PV_PhaseShift270 [KR] buffer=0.0
pv_PhaseShift270 :: UGen -> UGen
pv_PhaseShift270 buffer = mkUGen Nothing [KR] (Left KR) "PV_PhaseShift270" [buffer] Nothing 1 (Special 0) NoId

-- | Shift phase by 90 degrees.
--
--  PV_PhaseShift90 [KR] buffer=0.0
pv_PhaseShift90 :: UGen -> UGen
pv_PhaseShift90 buffer = mkUGen Nothing [KR] (Left KR) "PV_PhaseShift90" [buffer] Nothing 1 (Special 0) NoId

-- | Pass random bins.
--
--  PV_RandComb [KR] buffer=0.0 wipe=0.0 trig=0.0;    NONDET
pv_RandComb :: ID a => a -> UGen -> UGen -> UGen -> UGen
pv_RandComb z buffer wipe trig_ = mkUGen Nothing [KR] (Left KR) "PV_RandComb" [buffer,wipe,trig_] Nothing 1 (Special 0) (toUId z)

-- | Crossfade in random bin order.
--
--  PV_RandWipe [KR] bufferA=0.0 bufferB=0.0 wipe=0.0 trig=0.0;    NONDET
pv_RandWipe :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe z bufferA bufferB wipe trig_ = mkUGen Nothing [KR] (Left KR) "PV_RandWipe" [bufferA,bufferB,wipe,trig_] Nothing 1 (Special 0) (toUId z)

-- | Make gaps in spectrum.
--
--  PV_RectComb [KR] buffer=0.0 numTeeth=0.0 phase=0.0 width=0.5
pv_RectComb :: UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb buffer numTeeth phase width = mkUGen Nothing [KR] (Left KR) "PV_RectComb" [buffer,numTeeth,phase,width] Nothing 1 (Special 0) NoId

-- | Make gaps in spectrum.
--
--  PV_RectComb2 [KR] bufferA=0.0 bufferB=0.0 numTeeth=0.0 phase=0.0 width=0.5
pv_RectComb2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb2 bufferA bufferB numTeeth phase width = mkUGen Nothing [KR] (Left KR) "PV_RectComb2" [bufferA,bufferB,numTeeth,phase,width] Nothing 1 (Special 0) NoId

-- | Two channel equal power pan.
--
--  Pan2 [KR,AR] in=0.0 pos=0.0 level=1.0;    FILTER: TRUE
pan2 :: UGen -> UGen -> UGen -> UGen
pan2 in_ pos level = mkUGen Nothing [KR,AR] (Right [0]) "Pan2" [in_,pos,level] Nothing 2 (Special 0) NoId

-- | Four channel equal power pan.
--
--  Pan4 [KR,AR] in=0.0 xpos=0.0 ypos=0.0 level=1.0
pan4 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
pan4 rate in_ xpos ypos level = mkUGen Nothing [KR,AR] (Left rate) "Pan4" [in_,xpos,ypos,level] Nothing 4 (Special 0) NoId

-- | Azimuth panner
--
--  PanAz [KR,AR] in=0.0 pos=0.0 level=1.0 width=2.0 orientation=0.5;    NC INPUT: True, FILTER: TRUE
panAz :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
panAz numChannels in_ pos level width orientation = mkUGen Nothing [KR,AR] (Right [0]) "PanAz" [in_,pos,level,width,orientation] Nothing numChannels (Special 0) NoId

-- | Ambisonic B-format panner.
--
--  PanB [KR,AR] in=0.0 azimuth=0.0 elevation=0.0 gain=1.0
panB :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
panB rate in_ azimuth elevation gain = mkUGen Nothing [KR,AR] (Left rate) "PanB" [in_,azimuth,elevation,gain] Nothing 4 (Special 0) NoId

-- | 2D Ambisonic B-format panner.
--
--  PanB2 [KR,AR] in=0.0 azimuth=0.0 gain=1.0
panB2 :: Rate -> UGen -> UGen -> UGen -> UGen
panB2 rate in_ azimuth gain = mkUGen Nothing [KR,AR] (Left rate) "PanB2" [in_,azimuth,gain] Nothing 3 (Special 0) NoId

-- | Real-time partitioned convolution
--
--  PartConv [AR] in=0.0 fftsize=0.0 irbufnum=0.0
partConv :: UGen -> UGen -> UGen -> UGen
partConv in_ fftsize irbufnum = mkUGen Nothing [AR] (Left AR) "PartConv" [in_,fftsize,irbufnum] Nothing 1 (Special 0) NoId

-- | When triggered, pauses a node.
--
--  Pause [KR] gate=0.0 id=0.0
pause :: UGen -> UGen -> UGen
pause gate_ id_ = mkUGen Nothing [KR] (Left KR) "Pause" [gate_,id_] Nothing 1 (Special 0) NoId

-- | When triggered, pause enclosing synth.
--
--  PauseSelf [KR] in=0.0
pauseSelf :: UGen -> UGen
pauseSelf in_ = mkUGen Nothing [KR] (Left KR) "PauseSelf" [in_] Nothing 1 (Special 0) NoId

-- | FIXME: PauseSelfWhenDone purpose.
--
--  PauseSelfWhenDone [KR] src=0.0
pauseSelfWhenDone :: UGen -> UGen
pauseSelfWhenDone src = mkUGen Nothing [KR] (Left KR) "PauseSelfWhenDone" [src] Nothing 1 (Special 0) NoId

-- | Track peak signal amplitude.
--
--  Peak [KR,AR] in=0.0 trig=0.0;    FILTER: TRUE
peak :: UGen -> UGen -> UGen
peak in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "Peak" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Track peak signal amplitude.
--
--  PeakFollower [KR,AR] in=0.0 decay=0.999;    FILTER: TRUE
peakFollower :: UGen -> UGen -> UGen
peakFollower in_ decay_ = mkUGen Nothing [KR,AR] (Right [0]) "PeakFollower" [in_,decay_] Nothing 1 (Special 0) NoId

-- | A resettable linear ramp between two levels.
--
--  Phasor [KR,AR] trig=0.0 rate=1.0 start=0.0 end=1.0 resetPos=0.0
phasor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
phasor rate trig_ rate_ start end resetPos = mkUGen Nothing [KR,AR] (Left rate) "Phasor" [trig_,rate_,start,end,resetPos] Nothing 1 (Special 0) NoId

-- | Pink Noise.
--
--  PinkNoise [KR,AR] ;    NONDET
pinkNoise :: ID a => a -> Rate -> UGen
pinkNoise z rate = mkUGen Nothing [KR,AR] (Left rate) "PinkNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Autocorrelation pitch follower
--
--  Pitch [KR] in=0.0 initFreq=440.0 minFreq=60.0 maxFreq=4000.0 execFreq=100.0 maxBinsPerOctave=16.0 median=1.0 ampThreshold=1.0e-2 peakThreshold=0.5 downSample=1.0 clar=0.0
pitch :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pitch in_ initFreq minFreq maxFreq execFreq maxBinsPerOctave median_ ampThreshold peakThreshold downSample clar = mkUGen Nothing [KR] (Left KR) "Pitch" [in_,initFreq,minFreq,maxFreq,execFreq,maxBinsPerOctave,median_,ampThreshold,peakThreshold,downSample,clar] Nothing 2 (Special 0) NoId

-- | Time domain pitch shifter.
--
--  PitchShift [AR] in=0.0 windowSize=0.2 pitchRatio=1.0 pitchDispersion=0.0 timeDispersion=0.0;    FILTER: TRUE
pitchShift :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pitchShift in_ windowSize pitchRatio pitchDispersion timeDispersion = mkUGen Nothing [AR] (Right [0]) "PitchShift" [in_,windowSize,pitchRatio,pitchDispersion,timeDispersion] Nothing 1 (Special 0) NoId

-- | Sample playback oscillator.
--
--  PlayBuf [KR,AR] bufnum=0.0 rate=1.0 trigger=1.0 startPos=0.0 loop=0.0 doneAction=0.0;    NC INPUT: True, ENUMERATION INPUTS: 4=Loop, 5=DoneAction
playBuf :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> Loop -> DoneAction -> UGen
playBuf numChannels rate bufnum rate_ trigger startPos loop doneAction = mkUGen Nothing [KR,AR] (Left rate) "PlayBuf" [bufnum,rate_,trigger,startPos,(from_loop loop),(from_done_action doneAction)] Nothing numChannels (Special 0) NoId

-- | A Karplus-Strong UGen
--
--  Pluck [AR] in=0.0 trig=1.0 maxdelaytime=0.2 delaytime=0.2 decaytime=1.0 coef=0.5;    FILTER: TRUE
pluck :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pluck in_ trig_ maxdelaytime delaytime decaytime coef = mkUGen Nothing [AR] (Right [0]) "Pluck" [in_,trig_,maxdelaytime,delaytime,decaytime,coef] Nothing 1 (Special 0) NoId

-- | Band limited pulse wave.
--
--  Pulse [KR,AR] freq=440.0 width=0.5
pulse :: Rate -> UGen -> UGen -> UGen
pulse rate freq width = mkUGen Nothing [KR,AR] (Left rate) "Pulse" [freq,width] Nothing 1 (Special 0) NoId

-- | Pulse counter.
--
--  PulseCount [KR,AR] trig=0.0 reset=0.0;    FILTER: TRUE
pulseCount :: UGen -> UGen -> UGen
pulseCount trig_ reset = mkUGen Nothing [KR,AR] (Right [0]) "PulseCount" [trig_,reset] Nothing 1 (Special 0) NoId

-- | Pulse divider.
--
--  PulseDivider [KR,AR] trig=0.0 div=2.0 start=0.0;    FILTER: TRUE
pulseDivider :: UGen -> UGen -> UGen -> UGen
pulseDivider trig_ div_ start = mkUGen Nothing [KR,AR] (Right [0]) "PulseDivider" [trig_,div_,start] Nothing 1 (Special 0) NoId

-- | General quadratic map chaotic generator
--
--  QuadC [AR] freq=22050.0 a=1.0 b=-1.0 c=-0.75 xi=0.0
quadC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadC rate freq a b c xi = mkUGen Nothing [AR] (Left rate) "QuadC" [freq,a,b,c,xi] Nothing 1 (Special 0) NoId

-- | General quadratic map chaotic generator
--
--  QuadL [AR] freq=22050.0 a=1.0 b=-1.0 c=-0.75 xi=0.0
quadL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadL rate freq a b c xi = mkUGen Nothing [AR] (Left rate) "QuadL" [freq,a,b,c,xi] Nothing 1 (Special 0) NoId

-- | General quadratic map chaotic generator
--
--  QuadN [AR] freq=22050.0 a=1.0 b=-1.0 c=-0.75 xi=0.0
quadN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadN rate freq a b c xi = mkUGen Nothing [AR] (Left rate) "QuadN" [freq,a,b,c,xi] Nothing 1 (Special 0) NoId

-- | A resonant high pass filter.
--
--  RHPF [KR,AR] in=0.0 freq=440.0 rq=1.0;    FILTER: TRUE
rhpf :: UGen -> UGen -> UGen -> UGen
rhpf in_ freq rq = mkUGen Nothing [KR,AR] (Right [0]) "RHPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | A resonant low pass filter.
--
--  RLPF [KR,AR] in=0.0 freq=440.0 rq=1.0;    FILTER: TRUE
rlpf :: UGen -> UGen -> UGen -> UGen
rlpf in_ freq rq = mkUGen Nothing [KR,AR] (Right [0]) "RLPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Number of radians per sample.
--
--  RadiansPerSample [IR] 
radiansPerSample :: UGen
radiansPerSample = mkUGen Nothing [IR] (Left IR) "RadiansPerSample" [] Nothing 1 (Special 0) NoId

-- | Break a continuous signal into line segments
--
--  Ramp [KR,AR] in=0.0 lagTime=0.1;    FILTER: TRUE
ramp :: UGen -> UGen -> UGen
ramp in_ lagTime = mkUGen Nothing [KR,AR] (Right [0]) "Ramp" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Single random number generator.
--
--  Rand [IR] lo=0.0 hi=1.0;    NONDET
rand :: ID a => a -> UGen -> UGen -> UGen
rand z lo hi = mkUGen Nothing [IR] (Left IR) "Rand" [lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Set the synth's random generator ID.
--
--  RandID [IR,KR] id=0.0
randID :: Rate -> UGen -> UGen
randID rate id_ = mkUGen Nothing [IR,KR] (Left rate) "RandID" [id_] Nothing 0 (Special 0) NoId

-- | Sets the synth's random generator seed.
--
--  RandSeed [IR,KR,AR] trig=0.0 seed=56789.0
randSeed :: Rate -> UGen -> UGen -> UGen
randSeed rate trig_ seed = mkUGen Nothing [IR,KR,AR] (Left rate) "RandSeed" [trig_,seed] Nothing 0 (Special 0) NoId

-- | Record or overdub into a Buffer.
--
--  RecordBuf [KR,AR] bufnum=0.0 offset=0.0 recLevel=1.0 preLevel=0.0 run=1.0 loop=1.0 trigger=1.0 doneAction=0.0 *inputArray=0.0;    MCE, REORDERS INPUTS: [8,0,1,2,3,4,5,6,7], ENUMERATION INPUTS: 5=Loop, 7=DoneAction
recordBuf :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> Loop -> UGen -> DoneAction -> UGen -> UGen
recordBuf rate bufnum offset recLevel preLevel run loop trigger doneAction inputArray = mkUGen Nothing [KR,AR] (Left rate) "RecordBuf" [bufnum,offset,recLevel,preLevel,run,(from_loop loop),trigger,(from_done_action doneAction)] (Just inputArray) 1 (Special 0) NoId

-- | Send signal to a bus, overwriting previous contents.
--
--  ReplaceOut [KR,AR] bus=0.0 *channelsArray=0.0;    MCE, FILTER: TRUE
replaceOut :: UGen -> UGen -> UGen
replaceOut bus input = mkUGen Nothing [KR,AR] (Right [1]) "ReplaceOut" [bus] (Just input) 0 (Special 0) NoId

-- | Resonant filter.
--
--  Resonz [KR,AR] in=0.0 freq=440.0 bwr=1.0;    FILTER: TRUE
resonz :: UGen -> UGen -> UGen -> UGen
resonz in_ freq bwr = mkUGen Nothing [KR,AR] (Right [0]) "Resonz" [in_,freq,bwr] Nothing 1 (Special 0) NoId

-- | Ringing filter.
--
--  Ringz [KR,AR] in=0.0 freq=440.0 decaytime=1.0;    FILTER: TRUE
ringz :: UGen -> UGen -> UGen -> UGen
ringz in_ freq decaytime = mkUGen Nothing [KR,AR] (Right [0]) "Ringz" [in_,freq,decaytime] Nothing 1 (Special 0) NoId

-- | Rotate a sound field.
--
--  Rotate2 [KR,AR] x=0.0 y=0.0 pos=0.0;    FILTER: TRUE
rotate2 :: UGen -> UGen -> UGen -> UGen
rotate2 x y pos = mkUGen Nothing [KR,AR] (Right [0,1]) "Rotate2" [x,y,pos] Nothing 2 (Special 0) NoId

-- | Track maximum level.
--
--  RunningMax [KR,AR] in=0.0 trig=0.0;    FILTER: TRUE
runningMax :: UGen -> UGen -> UGen
runningMax in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "RunningMax" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Track minimum level.
--
--  RunningMin [KR,AR] in=0.0 trig=0.0;    FILTER: TRUE
runningMin :: UGen -> UGen -> UGen
runningMin in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "RunningMin" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Running sum over n frames
--
--  RunningSum [KR,AR] in=0.0 numsamp=40.0;    FILTER: TRUE
runningSum :: UGen -> UGen -> UGen
runningSum in_ numsamp = mkUGen Nothing [KR,AR] (Right [0]) "RunningSum" [in_,numsamp] Nothing 1 (Special 0) NoId

-- | Second order filter section (biquad).
--
--  SOS [KR,AR] in=0.0 a0=0.0 a1=0.0 a2=0.0 b1=0.0 b2=0.0;    FILTER: TRUE
sos :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sos in_ a0 a1 a2 b1 b2 = mkUGen Nothing [KR,AR] (Right [0]) "SOS" [in_,a0,a1,a2,b1,b2] Nothing 1 (Special 0) NoId

-- | Duration of one sample.
--
--  SampleDur [IR] 
sampleDur :: UGen
sampleDur = mkUGen Nothing [IR] (Left IR) "SampleDur" [] Nothing 1 (Special 0) NoId

-- | Server sample rate.
--
--  SampleRate [IR] 
sampleRate :: UGen
sampleRate = mkUGen Nothing [IR] (Left IR) "SampleRate" [] Nothing 1 (Special 0) NoId

-- | Band limited sawtooth.
--
--  Saw [KR,AR] freq=440.0
saw :: Rate -> UGen -> UGen
saw rate freq = mkUGen Nothing [KR,AR] (Left rate) "Saw" [freq] Nothing 1 (Special 0) NoId

-- | Schmidt trigger.
--
--  Schmidt [IR,KR,AR] in=0.0 lo=0.0 hi=1.0
schmidt :: UGen -> UGen -> UGen -> UGen
schmidt in_ lo hi = mkUGen Nothing [IR,KR,AR] (Right [0]) "Schmidt" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | FIXME: ScopeOut purpose.
--
--  ScopeOut [KR,AR] inputArray=0.0 bufnum=0.0
scopeOut :: Rate -> UGen -> UGen -> UGen
scopeOut rate inputArray bufnum = mkUGen Nothing [KR,AR] (Left rate) "ScopeOut" [inputArray,bufnum] Nothing 0 (Special 0) NoId

-- | (Undocumented class)
--
--  ScopeOut2 [KR,AR] inputArray=0.0 scopeNum=0.0 maxFrames=4096.0 scopeFrames=0.0
scopeOut2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
scopeOut2 rate inputArray scopeNum maxFrames scopeFrames = mkUGen Nothing [KR,AR] (Left rate) "ScopeOut2" [inputArray,scopeNum,maxFrames,scopeFrames] Nothing 0 (Special 0) NoId

-- | Select output from an array of inputs.
--
--  Select [IR,KR,AR] which=0.0 *array=0.0;    MCE, FILTER: TRUE
select :: UGen -> UGen -> UGen
select which array = mkUGen Nothing [IR,KR,AR] (Right [0,1]) "Select" [which] (Just array) 1 (Special 0) NoId

-- | Send a trigger message from the server back to the client.
--
--  SendTrig [KR,AR] in=0.0 id=0.0 value=0.0;    FILTER: TRUE
sendTrig :: UGen -> UGen -> UGen -> UGen
sendTrig in_ id_ value = mkUGen Nothing [KR,AR] (Right [0]) "SendTrig" [in_,id_,value] Nothing 0 (Special 0) NoId

-- | Set-reset flip flop.
--
--  SetResetFF [KR,AR] trig=0.0 reset=0.0;    FILTER: TRUE
setResetFF :: UGen -> UGen -> UGen
setResetFF trig_ reset = mkUGen Nothing [KR,AR] (Right [0]) "SetResetFF" [trig_,reset] Nothing 1 (Special 0) NoId

-- | Wave shaper.
--
--  Shaper [KR,AR] bufnum=0.0 in=0.0;    FILTER: TRUE
shaper :: UGen -> UGen -> UGen
shaper bufnum in_ = mkUGen Nothing [KR,AR] (Right [1]) "Shaper" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Interpolating sine wavetable oscillator.
--
--  SinOsc [KR,AR] freq=440.0 phase=0.0
sinOsc :: Rate -> UGen -> UGen -> UGen
sinOsc rate freq phase = mkUGen Nothing [KR,AR] (Left rate) "SinOsc" [freq,phase] Nothing 1 (Special 0) NoId

-- | Feedback FM oscillator
--
--  SinOscFB [KR,AR] freq=440.0 feedback=0.0
sinOscFB :: Rate -> UGen -> UGen -> UGen
sinOscFB rate freq feedback = mkUGen Nothing [KR,AR] (Left rate) "SinOscFB" [freq,feedback] Nothing 1 (Special 0) NoId

-- | Slew rate limiter.
--
--  Slew [KR,AR] in=0.0 up=1.0 dn=1.0;    FILTER: TRUE
slew :: UGen -> UGen -> UGen -> UGen
slew in_ up dn = mkUGen Nothing [KR,AR] (Right [0]) "Slew" [in_,up,dn] Nothing 1 (Special 0) NoId

-- | Slope of signal
--
--  Slope [KR,AR] in=0.0;    FILTER: TRUE
slope :: UGen -> UGen
slope in_ = mkUGen Nothing [KR,AR] (Right [0]) "Slope" [in_] Nothing 1 (Special 0) NoId

-- | Spectral centroid
--
--  SpecCentroid [KR] buffer=0.0
specCentroid :: Rate -> UGen -> UGen
specCentroid rate buffer = mkUGen Nothing [KR] (Left rate) "SpecCentroid" [buffer] Nothing 1 (Special 0) NoId

-- | Spectral Flatness measure
--
--  SpecFlatness [KR] buffer=0.0
specFlatness :: Rate -> UGen -> UGen
specFlatness rate buffer = mkUGen Nothing [KR] (Left rate) "SpecFlatness" [buffer] Nothing 1 (Special 0) NoId

-- | Find a percentile of FFT magnitude spectrum
--
--  SpecPcile [KR] buffer=0.0 fraction=0.5 interpolate=0.0
specPcile :: Rate -> UGen -> UGen -> UGen -> UGen
specPcile rate buffer fraction interpolate = mkUGen Nothing [KR] (Left rate) "SpecPcile" [buffer,fraction,interpolate] Nothing 1 (Special 0) NoId

-- | physical model of resonating spring
--
--  Spring [KR,AR] in=0.0 spring=1.0 damp=0.0
spring :: Rate -> UGen -> UGen -> UGen -> UGen
spring rate in_ spring_ damp = mkUGen Nothing [KR,AR] (Left rate) "Spring" [in_,spring_,damp] Nothing 1 (Special 0) NoId

-- | Standard map chaotic generator
--
--  StandardL [AR] freq=22050.0 k=1.0 xi=0.5 yi=0.0
standardL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
standardL rate freq k xi yi = mkUGen Nothing [AR] (Left rate) "StandardL" [freq,k,xi,yi] Nothing 1 (Special 0) NoId

-- | Standard map chaotic generator
--
--  StandardN [AR] freq=22050.0 k=1.0 xi=0.5 yi=0.0
standardN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
standardN rate freq k xi yi = mkUGen Nothing [AR] (Left rate) "StandardN" [freq,k,xi,yi] Nothing 1 (Special 0) NoId

-- | Pulse counter.
--
--  Stepper [KR,AR] trig=0.0 reset=0.0 min=0.0 max=7.0 step=1.0 resetval=0.0;    FILTER: TRUE
stepper :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stepper trig_ reset min_ max_ step resetval = mkUGen Nothing [KR,AR] (Right [0]) "Stepper" [trig_,reset,min_,max_,step,resetval] Nothing 1 (Special 0) NoId

-- | Stereo real-time convolver with linear interpolation
--
--  StereoConvolution2L [AR] in=0.0 kernelL=0.0 kernelR=0.0 trigger=0.0 framesize=2048.0 crossfade=1.0
stereoConvolution2L :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stereoConvolution2L rate in_ kernelL kernelR trigger framesize crossfade = mkUGen Nothing [AR] (Left rate) "StereoConvolution2L" [in_,kernelL,kernelR,trigger,framesize,crossfade] Nothing 2 (Special 0) NoId

-- | Offset from synth start within one sample.
--
--  SubsampleOffset [IR] 
subsampleOffset :: UGen
subsampleOffset = mkUGen Nothing [IR] (Left IR) "SubsampleOffset" [] Nothing 1 (Special 0) NoId

-- | Sum three signals
--
--  Sum3 [] in0=0.0 in1=0.0 in2=0.0;    FILTER: TRUE
sum3 :: UGen -> UGen -> UGen -> UGen
sum3 in0 in1 in2 = mkUGen Nothing [IR,KR,AR,DR] (Right [0,1,2]) "Sum3" [in0,in1,in2] Nothing 1 (Special 0) NoId

-- | Sum four signals
--
--  Sum4 [] in0=0.0 in1=0.0 in2=0.0 in3=0.0;    FILTER: TRUE
sum4 :: UGen -> UGen -> UGen -> UGen -> UGen
sum4 in0 in1 in2 in3 = mkUGen Nothing [IR,KR,AR,DR] (Right [0,1,2,3]) "Sum4" [in0,in1,in2,in3] Nothing 1 (Special 0) NoId

-- | Triggered linear ramp
--
--  Sweep [KR,AR] trig=0.0 rate=1.0;    FILTER: TRUE
sweep :: UGen -> UGen -> UGen
sweep trig_ rate_ = mkUGen Nothing [KR,AR] (Right [0]) "Sweep" [trig_,rate_] Nothing 1 (Special 0) NoId

-- | Hard sync sawtooth wave.
--
--  SyncSaw [KR,AR] syncFreq=440.0 sawFreq=440.0
syncSaw :: Rate -> UGen -> UGen -> UGen
syncSaw rate syncFreq sawFreq = mkUGen Nothing [KR,AR] (Left rate) "SyncSaw" [syncFreq,sawFreq] Nothing 1 (Special 0) NoId

-- | Control rate trigger to audio rate trigger converter
--
--  T2A [AR] in=0.0 offset=0.0
t2A :: UGen -> UGen -> UGen
t2A in_ offset = mkUGen Nothing [AR] (Left AR) "T2A" [in_,offset] Nothing 1 (Special 0) NoId

-- | Audio rate trigger to control rate trigger converter
--
--  T2K [KR] in=0.0
t2K :: Rate -> UGen -> UGen
t2K rate in_ = mkUGen Nothing [KR] (Left rate) "T2K" [in_] Nothing 1 (Special 0) NoId

-- | physical model of bouncing object
--
--  TBall [KR,AR] in=0.0 g=10.0 damp=0.0 friction=1.0e-2
tBall :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
tBall rate in_ g damp friction = mkUGen Nothing [KR,AR] (Left rate) "TBall" [in_,g,damp,friction] Nothing 1 (Special 0) NoId

-- | Trigger delay.
--
--  TDelay [KR,AR] in=0.0 dur=0.1;    FILTER: TRUE
tDelay :: UGen -> UGen -> UGen
tDelay in_ dur = mkUGen Nothing [KR,AR] (Right [0]) "TDelay" [in_,dur] Nothing 1 (Special 0) NoId

-- | Demand results as trigger from demand rate UGens.
--
--  TDuty [KR,AR] dur=1.0 reset=0.0 doneAction=0.0 level=1.0 gapFirst=0.0;    REORDERS INPUTS: [0,1,3,2,4], ENUMERATION INPUTS: 2=DoneAction
tDuty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen -> UGen
tDuty rate dur reset doneAction level gapFirst = mkUGen Nothing [KR,AR] (Left rate) "TDuty" [dur,reset,(from_done_action doneAction),level,gapFirst] Nothing 1 (Special 0) NoId

-- | Triggered exponential random number generator.
--
--  TExpRand [KR,AR] lo=1.0e-2 hi=1.0 trig=0.0;    FILTER: TRUE, NONDET
tExpRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tExpRand z lo hi trig_ = mkUGen Nothing [KR,AR] (Right [2]) "TExpRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | Buffer granulator.
--
--  TGrains [AR] trigger=0.0 bufnum=0.0 rate=1.0 centerPos=0.0 dur=0.1 pan=0.0 amp=0.1 interp=4.0;    NC INPUT: True
tGrains :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains numChannels trigger bufnum rate_ centerPos dur pan amp interp = mkUGen Nothing [AR] (Left AR) "TGrains" [trigger,bufnum,rate_,centerPos,dur,pan,amp,interp] Nothing numChannels (Special 0) NoId

-- | Triggered integer random number generator.
--
--  TIRand [KR,AR] lo=0.0 hi=127.0 trig=0.0;    FILTER: TRUE, NONDET
tIRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tIRand z lo hi trig_ = mkUGen Nothing [KR,AR] (Right [2]) "TIRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | Triggered random number generator.
--
--  TRand [KR,AR] lo=0.0 hi=1.0 trig=0.0;    FILTER: TRUE, NONDET
tRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tRand z lo hi trig_ = mkUGen Nothing [KR,AR] (Right [2]) "TRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | Triggered windex.
--
--  TWindex [KR,AR] in=0.0 normalize=0.0 *array=0.0;    MCE, FILTER: TRUE, REORDERS INPUTS: [0,2,1], NONDET
tWindex :: ID a => a -> UGen -> UGen -> UGen -> UGen
tWindex z in_ normalize array = mkUGen Nothing [KR,AR] (Right [0]) "TWindex" [in_,normalize] (Just array) 1 (Special 0) (toUId z)

-- | Returns time since last triggered.
--
--  Timer [KR,AR] trig=0.0;    FILTER: TRUE
timer :: UGen -> UGen
timer trig_ = mkUGen Nothing [KR,AR] (Right [0]) "Timer" [trig_] Nothing 1 (Special 0) NoId

-- | Toggle flip flop.
--
--  ToggleFF [KR,AR] trig=0.0;    FILTER: TRUE
toggleFF :: UGen -> UGen
toggleFF trig_ = mkUGen Nothing [KR,AR] (Right [0]) "ToggleFF" [trig_] Nothing 1 (Special 0) NoId

-- | Timed trigger.
--
--  Trig [KR,AR] in=0.0 dur=0.1;    FILTER: TRUE
trig :: UGen -> UGen -> UGen
trig in_ dur = mkUGen Nothing [KR,AR] (Right [0]) "Trig" [in_,dur] Nothing 1 (Special 0) NoId

-- | Timed trigger.
--
--  Trig1 [KR,AR] in=0.0 dur=0.1;    FILTER: TRUE
trig1 :: UGen -> UGen -> UGen
trig1 in_ dur = mkUGen Nothing [KR,AR] (Right [0]) "Trig1" [in_,dur] Nothing 1 (Special 0) NoId

-- | FIXME: TrigControl purpose.
--
--  TrigControl [IR,KR] values=0.0
trigControl :: Rate -> UGen -> UGen
trigControl rate values = mkUGen Nothing [IR,KR] (Left rate) "TrigControl" [values] Nothing 0 (Special 0) NoId

-- | Two pole filter.
--
--  TwoPole [KR,AR] in=0.0 freq=440.0 radius=0.8;    FILTER: TRUE
twoPole :: UGen -> UGen -> UGen -> UGen
twoPole in_ freq radius = mkUGen Nothing [KR,AR] (Right [0]) "TwoPole" [in_,freq,radius] Nothing 1 (Special 0) NoId

-- | Two zero filter.
--
--  TwoZero [KR,AR] in=0.0 freq=440.0 radius=0.8;    FILTER: TRUE
twoZero :: UGen -> UGen -> UGen -> UGen
twoZero in_ freq radius = mkUGen Nothing [KR,AR] (Right [0]) "TwoZero" [in_,freq,radius] Nothing 1 (Special 0) NoId

-- | Apply a unary operation to the values of an input ugen
--
--  UnaryOpUGen [] a=0.0;    FILTER: TRUE
unaryOpUGen :: UGen -> UGen
unaryOpUGen a = mkUGen Nothing [IR,KR,AR,DR] (Right [0]) "UnaryOpUGen" [a] Nothing 1 (Special 0) NoId

-- | Stream in audio from a file, with variable rate
--
--  VDiskIn [AR] bufnum=0.0 rate=1.0 loop=0.0 sendID=0.0;    NC INPUT: True, ENUMERATION INPUTS: 2=Loop
vDiskIn :: Int -> UGen -> UGen -> Loop -> UGen -> UGen
vDiskIn numChannels bufnum rate_ loop sendID = mkUGen Nothing [AR] (Left AR) "VDiskIn" [bufnum,rate_,(from_loop loop),sendID] Nothing numChannels (Special 0) NoId

-- | Variable wavetable oscillator.
--
--  VOsc [KR,AR] bufpos=0.0 freq=440.0 phase=0.0
vOsc :: Rate -> UGen -> UGen -> UGen -> UGen
vOsc rate bufpos freq phase = mkUGen Nothing [KR,AR] (Left rate) "VOsc" [bufpos,freq,phase] Nothing 1 (Special 0) NoId

-- | Three variable wavetable oscillators.
--
--  VOsc3 [KR,AR] bufpos=0.0 freq1=110.0 freq2=220.0 freq3=440.0
vOsc3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vOsc3 rate bufpos freq1 freq2 freq3 = mkUGen Nothing [KR,AR] (Left rate) "VOsc3" [bufpos,freq1,freq2,freq3] Nothing 1 (Special 0) NoId

-- | Variable shaped lag
--
--  VarLag [KR,AR] in=0.0 time=0.1 curvature=0.0 warp=5.0 start=0.0
varLag :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
varLag in_ time curvature warp start = mkUGen Nothing [KR,AR] (Right [0]) "VarLag" [in_,time,curvature,warp,start] Nothing 0 (Special 0) NoId

-- | Variable duty saw
--
--  VarSaw [KR,AR] freq=440.0 iphase=0.0 width=0.5
varSaw :: Rate -> UGen -> UGen -> UGen -> UGen
varSaw rate freq iphase width = mkUGen Nothing [KR,AR] (Left rate) "VarSaw" [freq,iphase,width] Nothing 1 (Special 0) NoId

-- | The Vibrato oscillator models a slow frequency modulation.
--
--  Vibrato [KR,AR] freq=440.0 rate=6.0 depth=2.0e-2 delay=0.0 onset=0.0 rateVariation=4.0e-2 depthVariation=0.1 iphase=0.0;    NONDET
vibrato :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
vibrato z rate freq rate_ depth delay onset rateVariation depthVariation iphase = mkUGen Nothing [KR,AR] (Left rate) "Vibrato" [freq,rate_,depth,delay,onset,rateVariation,depthVariation,iphase] Nothing 1 (Special 0) (toUId z)

-- | Warp a buffer with a time pointer
--
--  Warp1 [AR] bufnum=0.0 pointer=0.0 freqScale=1.0 windowSize=0.2 envbufnum=-1.0 overlaps=8.0 windowRandRatio=0.0 interp=1.0;    NC INPUT: True
warp1 :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
warp1 numChannels bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp = mkUGen Nothing [AR] (Left AR) "Warp1" [bufnum,pointer,freqScale,windowSize,envbufnum,overlaps,windowRandRatio,interp] Nothing numChannels (Special 0) NoId

-- | White noise.
--
--  WhiteNoise [KR,AR] ;    NONDET
whiteNoise :: ID a => a -> Rate -> UGen
whiteNoise z rate = mkUGen Nothing [KR,AR] (Left rate) "WhiteNoise" [] Nothing 1 (Special 0) (toUId z)

-- | (Undocumented class)
--
--  WidthFirstUGen [] maxSize=0.0
widthFirstUGen :: Rate -> UGen -> UGen
widthFirstUGen rate maxSize = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "WidthFirstUGen" [maxSize] Nothing 1 (Special 0) NoId

-- | Wrap a signal outside given thresholds.
--
--  Wrap [IR,KR,AR] in=0.0 lo=0.0 hi=1.0;    FILTER: TRUE
wrap :: UGen -> UGen -> UGen -> UGen
wrap in_ lo hi = mkUGen Nothing [IR,KR,AR] (Right [0]) "Wrap" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Index into a table with a signal.
--
--  WrapIndex [KR,AR] bufnum=0.0 in=0.0;    FILTER: TRUE
wrapIndex :: UGen -> UGen -> UGen
wrapIndex bufnum in_ = mkUGen Nothing [KR,AR] (Right [1]) "WrapIndex" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Equal power two channel cross fade.
--
--  XFade2 [KR,AR] inA=0.0 inB=0.0 pan=0.0 level=1.0;    FILTER: TRUE
xFade2 :: UGen -> UGen -> UGen -> UGen -> UGen
xFade2 inA inB pan level = mkUGen Nothing [KR,AR] (Right [0,1]) "XFade2" [inA,inB,pan,level] Nothing 1 (Special 0) NoId

-- | Exponential line generator.
--
--  XLine [KR,AR] start=1.0 end=2.0 dur=1.0 doneAction=0.0;    ENUMERATION INPUTS: 3=DoneAction
xLine :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
xLine rate start end dur doneAction = mkUGen Nothing [KR,AR] (Left rate) "XLine" [start,end,dur,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Send signal to a bus, crossfading with previous contents.
--
--  XOut [KR,AR] bus=0.0 xfade=0.0 *channelsArray=0.0;    MCE, FILTER: TRUE
xOut :: UGen -> UGen -> UGen -> UGen
xOut bus xfade input = mkUGen Nothing [KR,AR] (Right [2]) "XOut" [bus,xfade] (Just input) 0 (Special 0) NoId

-- | Zero crossing frequency follower
--
--  ZeroCrossing [KR,AR] in=0.0;    FILTER: TRUE
zeroCrossing :: UGen -> UGen
zeroCrossing in_ = mkUGen Nothing [KR,AR] (Right [0]) "ZeroCrossing" [in_] Nothing 1 (Special 0) NoId

-- | LocalBuf count
--
--  MaxLocalBufs [IR] count=0.0
maxLocalBufs :: Rate -> UGen -> UGen
maxLocalBufs rate count = mkUGen Nothing [IR] (Left rate) "MaxLocalBufs" [count] Nothing 1 (Special 0) NoId

-- | Multiply add
--
--  MulAdd [IR,KR,AR] in=0.0 mul=0.0 add=0.0;    FILTER: TRUE
mulAdd :: UGen -> UGen -> UGen -> UGen
mulAdd in_ mul add = mkUGen Nothing [IR,KR,AR] (Right [0]) "MulAdd" [in_,mul,add] Nothing 1 (Special 0) NoId

-- | Set local buffer
--
--  SetBuf [IR] buf=0.0 offset=0.0 length=0.0 *array=0.0;    MCE, REORDERS INPUTS: [0,1,2,3]
setBuf :: UGen -> UGen -> UGen -> UGen -> UGen
setBuf buf offset length_ array = mkUGen Nothing [IR] (Left IR) "SetBuf" [buf,offset,length_] (Just array) 1 (Special 0) NoId
