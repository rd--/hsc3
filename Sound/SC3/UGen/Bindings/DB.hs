-- | SC3 UGen bindings (auto-generated).
module Sound.SC3.UGen.Bindings.DB where

import Sound.SC3.Common.Enum
import Sound.SC3.Common.Envelope
import Sound.SC3.Common.Rate
import Sound.SC3.Common.UId
import Sound.SC3.Common.Unsafe
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Apply a binary operation to the values of an input UGen
--
--  BinaryOpUGen [InitialisationRate,ControlRate,AudioRate,DemandRate] a=0 b=0;    FILTER: TRUE
binaryOpUGen :: UGen -> UGen -> UGen
binaryOpUGen a b = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Right [0,1]) "BinaryOpUGen" [a,b] Nothing 1 (Special 0) NoId

-- | Apply a unary operation to the values of an input ugen
--
--  UnaryOpUGen [InitialisationRate,ControlRate,AudioRate,DemandRate] a=0;    FILTER: TRUE
unaryOpUGen :: UGen -> UGen
unaryOpUGen a = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Right [0]) "UnaryOpUGen" [a] Nothing 1 (Special 0) NoId

-- | Audio to control rate converter.
--
--  A2K [ControlRate] in=0
a2k :: UGen -> UGen
a2k in_ = mkUGen Nothing [ControlRate] (Left ControlRate) "A2K" [in_] Nothing 1 (Special 0) NoId

-- | FIXME: APF purpose.
--
--  APF [ControlRate,AudioRate] in=0 freq=440 radius=0.8;    FILTER: TRUE
apf :: UGen -> UGen -> UGen -> UGen
apf in_ freq radius = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "APF" [in_,freq,radius] Nothing 1 (Special 0) NoId

-- | Schroeder allpass delay line with cubic interpolation.
--
--  AllpassC [ControlRate,AudioRate] in=0 maxdelaytime=0.2 delaytime=0.2 decaytime=1;    FILTER: TRUE
allpassC :: UGen -> UGen -> UGen -> UGen -> UGen
allpassC in_ maxdelaytime delaytime decaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "AllpassC" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Schroeder allpass delay line with linear interpolation.
--
--  AllpassL [ControlRate,AudioRate] in=0 maxdelaytime=0.2 delaytime=0.2 decaytime=1;    FILTER: TRUE
allpassL :: UGen -> UGen -> UGen -> UGen -> UGen
allpassL in_ maxdelaytime delaytime decaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "AllpassL" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Schroeder allpass delay line with no interpolation.
--
--  AllpassN [ControlRate,AudioRate] in=0 maxdelaytime=0.2 delaytime=0.2 decaytime=1;    FILTER: TRUE
allpassN :: UGen -> UGen -> UGen -> UGen -> UGen
allpassN in_ maxdelaytime delaytime decaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "AllpassN" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Basic psychoacoustic amplitude compensation.
--
--  AmpComp [InitialisationRate,ControlRate,AudioRate] freq=0 root=0 exp=0.3333
ampComp :: Rate -> UGen -> UGen -> UGen -> UGen
ampComp rate freq root exp_ = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Left rate) "AmpComp" [freq,root,exp_] Nothing 1 (Special 0) NoId

-- | Basic psychoacoustic amplitude compensation (ANSI A-weighting curve).
--
--  AmpCompA [InitialisationRate,ControlRate,AudioRate] freq=1000 root=0 minAmp=0.32 rootAmp=1
ampCompA :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
ampCompA rate freq root minAmp rootAmp = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Left rate) "AmpCompA" [freq,root,minAmp,rootAmp] Nothing 1 (Special 0) NoId

-- | Amplitude follower
--
--  Amplitude [ControlRate,AudioRate] in=0 attackTime=0.01 releaseTime=0.01
amplitude :: Rate -> UGen -> UGen -> UGen -> UGen
amplitude rate in_ attackTime releaseTime = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Amplitude" [in_,attackTime,releaseTime] Nothing 1 (Special 0) NoId

-- | All Pass Filter
--
--  BAllPass [AudioRate] in=0 freq=1200 rq=1;    FILTER: TRUE
bAllPass :: UGen -> UGen -> UGen -> UGen
bAllPass in_ freq rq = mkUGen Nothing [AudioRate] (Right [0]) "BAllPass" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Band Pass Filter
--
--  BBandPass [AudioRate] in=0 freq=1200 bw=1;    FILTER: TRUE
bBandPass :: UGen -> UGen -> UGen -> UGen
bBandPass in_ freq bw = mkUGen Nothing [AudioRate] (Right [0]) "BBandPass" [in_,freq,bw] Nothing 1 (Special 0) NoId

-- | Band reject filter
--
--  BBandStop [AudioRate] in=0 freq=1200 bw=1;    FILTER: TRUE
bBandStop :: UGen -> UGen -> UGen -> UGen
bBandStop in_ freq bw = mkUGen Nothing [AudioRate] (Right [0]) "BBandStop" [in_,freq,bw] Nothing 1 (Special 0) NoId

-- | 12db/oct rolloff - 2nd order resonant  Hi Pass Filter
--
--  BHiPass [AudioRate] in=0 freq=1200 rq=1;    FILTER: TRUE
bHiPass :: UGen -> UGen -> UGen -> UGen
bHiPass in_ freq rq = mkUGen Nothing [AudioRate] (Right [0]) "BHiPass" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Hi Shelf
--
--  BHiShelf [AudioRate] in=0 freq=1200 rs=1 db=0;    FILTER: TRUE
bHiShelf :: UGen -> UGen -> UGen -> UGen -> UGen
bHiShelf in_ freq rs db = mkUGen Nothing [AudioRate] (Right [0]) "BHiShelf" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | 12db/oct rolloff - 2nd order resonant Low Pass Filter
--
--  BLowPass [AudioRate] in=0 freq=1200 rq=1;    FILTER: TRUE
bLowPass :: UGen -> UGen -> UGen -> UGen
bLowPass in_ freq rq = mkUGen Nothing [AudioRate] (Right [0]) "BLowPass" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Low Shelf
--
--  BLowShelf [AudioRate] in=0 freq=1200 rs=1 db=0;    FILTER: TRUE
bLowShelf :: UGen -> UGen -> UGen -> UGen -> UGen
bLowShelf in_ freq rs db = mkUGen Nothing [AudioRate] (Right [0]) "BLowShelf" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | 2nd order Butterworth bandpass filter.
--
--  BPF [ControlRate,AudioRate] in=0 freq=440 rq=1;    FILTER: TRUE
bpf :: UGen -> UGen -> UGen -> UGen
bpf in_ freq rq = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "BPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Two zero fixed midpass.
--
--  BPZ2 [ControlRate,AudioRate] in=0;    FILTER: TRUE
bpz2 :: UGen -> UGen
bpz2 in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "BPZ2" [in_] Nothing 1 (Special 0) NoId

-- | Parametric equalizer
--
--  BPeakEQ [AudioRate] in=0 freq=1200 rq=1 db=0;    FILTER: TRUE
bPeakEQ :: UGen -> UGen -> UGen -> UGen -> UGen
bPeakEQ in_ freq rq db = mkUGen Nothing [AudioRate] (Right [0]) "BPeakEQ" [in_,freq,rq,db] Nothing 1 (Special 0) NoId

-- | 2nd order Butterworth band reject filter.
--
--  BRF [ControlRate,AudioRate] in=0 freq=440 rq=1;    FILTER: TRUE
brf :: UGen -> UGen -> UGen -> UGen
brf in_ freq rq = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "BRF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Two zero fixed midcut.
--
--  BRZ2 [ControlRate,AudioRate] in=0;    FILTER: TRUE
brz2 :: UGen -> UGen
brz2 in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "BRZ2" [in_] Nothing 1 (Special 0) NoId

-- | Stereo signal balancer
--
--  Balance2 [ControlRate,AudioRate] left=0 right=0 pos=0 level=1;    FILTER: TRUE
balance2 :: UGen -> UGen -> UGen -> UGen -> UGen
balance2 left right pos level = mkUGen Nothing [ControlRate,AudioRate] (Right [0,1]) "Balance2" [left,right,pos,level] Nothing 2 (Special 0) NoId

-- | physical model of bouncing object
--
--  Ball [ControlRate,AudioRate] in=0 g=1 damp=0 friction=0.01
ball :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
ball rate in_ g damp friction_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Ball" [in_,g,damp,friction_] Nothing 1 (Special 0) NoId

-- | Autocorrelation beat tracker
--
--  BeatTrack [ControlRate] chain=0 lock=0
beatTrack :: Rate -> UGen -> UGen -> UGen
beatTrack rate chain lock = mkUGen Nothing [ControlRate] (Left rate) "BeatTrack" [chain,lock] Nothing 4 (Special 0) NoId

-- | Template matching beat tracker
--
--  BeatTrack2 [ControlRate] busindex=0 numfeatures=0 windowsize=2 phaseaccuracy=0.02 lock=0 weightingscheme=0
beatTrack2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
beatTrack2 rate busindex numfeatures windowsize phaseaccuracy lock weightingscheme = mkUGen Nothing [ControlRate] (Left rate) "BeatTrack2" [busindex,numfeatures,windowsize,phaseaccuracy,lock,weightingscheme] Nothing 6 (Special 0) NoId

-- | 2D Ambisonic B-format panner.
--
--  BiPanB2 [ControlRate,AudioRate] inA=0 inB=0 azimuth=0 gain=1
biPanB2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
biPanB2 rate inA inB azimuth gain = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "BiPanB2" [inA,inB,azimuth,gain] Nothing 3 (Special 0) NoId

-- | Band limited impulse oscillator.
--
--  Blip [ControlRate,AudioRate] freq=440 numharm=200
blip :: Rate -> UGen -> UGen -> UGen
blip rate freq numharm = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Blip" [freq,numharm] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BlockSize [InitialisationRate]
blockSize :: UGen
blockSize = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "BlockSize" [] Nothing 1 (Special 0) NoId

-- | Brown Noise.
--
--  BrownNoise [ControlRate,AudioRate] ;    NONDET
brownNoiseId :: ID a => a -> Rate -> UGen
brownNoiseId z rate = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "BrownNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of BrownNoise.
brownNoiseM :: UId m => Rate -> m UGen
brownNoiseM = liftUId1 brownNoiseId

-- | Unsafe variant of BrownNoise.
brownNoise ::  Rate -> UGen
brownNoise = liftUnsafe1 brownNoiseM

-- | Buffer based all pass delay line with cubic interpolation.
--
--  BufAllpassC [AudioRate] buf=0 in=0 delaytime=0.2 decaytime=1;    FILTER: TRUE
bufAllpassC :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassC buf in_ delaytime decaytime = mkUGen Nothing [AudioRate] (Right [1]) "BufAllpassC" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based all pass delay line with linear interpolation.
--
--  BufAllpassL [AudioRate] buf=0 in=0 delaytime=0.2 decaytime=1;    FILTER: TRUE
bufAllpassL :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassL buf in_ delaytime decaytime = mkUGen Nothing [AudioRate] (Right [1]) "BufAllpassL" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based all pass delay line with no interpolation.
--
--  BufAllpassN [AudioRate] buf=0 in=0 delaytime=0.2 decaytime=1;    FILTER: TRUE
bufAllpassN :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassN buf in_ delaytime decaytime = mkUGen Nothing [AudioRate] (Right [1]) "BufAllpassN" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Current number of channels of soundfile in buffer.
--
--  BufChannels [InitialisationRate,ControlRate] bufnum=0
bufChannels :: Rate -> UGen -> UGen
bufChannels rate bufnum = mkUGen Nothing [InitialisationRate,ControlRate] (Left rate) "BufChannels" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer based comb delay line with cubic interpolation.
--
--  BufCombC [AudioRate] buf=0 in=0 delaytime=0.2 decaytime=1;    FILTER: TRUE
bufCombC :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombC buf in_ delaytime decaytime = mkUGen Nothing [AudioRate] (Right [1]) "BufCombC" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based comb delay line with linear interpolation.
--
--  BufCombL [AudioRate] buf=0 in=0 delaytime=0.2 decaytime=1;    FILTER: TRUE
bufCombL :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombL buf in_ delaytime decaytime = mkUGen Nothing [AudioRate] (Right [1]) "BufCombL" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based comb delay line with no interpolation.
--
--  BufCombN [AudioRate] buf=0 in=0 delaytime=0.2 decaytime=1;    FILTER: TRUE
bufCombN :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombN buf in_ delaytime decaytime = mkUGen Nothing [AudioRate] (Right [1]) "BufCombN" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based simple delay line with cubic interpolation.
--
--  BufDelayC [ControlRate,AudioRate] buf=0 in=0 delaytime=0.2;    FILTER: TRUE
bufDelayC :: UGen -> UGen -> UGen -> UGen
bufDelayC buf in_ delaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "BufDelayC" [buf,in_,delaytime] Nothing 1 (Special 0) NoId

-- | Buffer based simple delay line with linear interpolation.
--
--  BufDelayL [ControlRate,AudioRate] buf=0 in=0 delaytime=0.2;    FILTER: TRUE
bufDelayL :: UGen -> UGen -> UGen -> UGen
bufDelayL buf in_ delaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "BufDelayL" [buf,in_,delaytime] Nothing 1 (Special 0) NoId

-- | Buffer based simple delay line with no interpolation.
--
--  BufDelayN [ControlRate,AudioRate] buf=0 in=0 delaytime=0.2;    FILTER: TRUE
bufDelayN :: UGen -> UGen -> UGen -> UGen
bufDelayN buf in_ delaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "BufDelayN" [buf,in_,delaytime] Nothing 1 (Special 0) NoId

-- | Current duration of soundfile in buffer.
--
--  BufDur [InitialisationRate,ControlRate] bufnum=0
bufDur :: Rate -> UGen -> UGen
bufDur rate bufnum = mkUGen Nothing [InitialisationRate,ControlRate] (Left rate) "BufDur" [bufnum] Nothing 1 (Special 0) NoId

-- | Current number of frames allocated in the buffer.
--
--  BufFrames [InitialisationRate,ControlRate] bufnum=0
bufFrames :: Rate -> UGen -> UGen
bufFrames rate bufnum = mkUGen Nothing [InitialisationRate,ControlRate] (Left rate) "BufFrames" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer rate scaling in respect to server samplerate.
--
--  BufRateScale [InitialisationRate,ControlRate] bufnum=0
bufRateScale :: Rate -> UGen -> UGen
bufRateScale rate bufnum = mkUGen Nothing [InitialisationRate,ControlRate] (Left rate) "BufRateScale" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer reading oscillator.
--
--  BufRd [ControlRate,AudioRate] bufnum=0 phase=0 loop=1 interpolation=2;    NC INPUT: True, ENUMERATION INPUTS: 2=Loop, 3=Interpolation
bufRd :: Int -> Rate -> UGen -> UGen -> Loop UGen -> Interpolation UGen -> UGen
bufRd numChannels rate bufnum phase loop interpolation = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "BufRd" [bufnum,phase,(from_loop loop),(from_interpolation interpolation)] Nothing numChannels (Special 0) NoId

-- | Buffer sample rate.
--
--  BufSampleRate [InitialisationRate,ControlRate] bufnum=0
bufSampleRate :: Rate -> UGen -> UGen
bufSampleRate rate bufnum = mkUGen Nothing [InitialisationRate,ControlRate] (Left rate) "BufSampleRate" [bufnum] Nothing 1 (Special 0) NoId

-- | Current number of samples in buffer.
--
--  BufSamples [InitialisationRate,ControlRate] bufnum=0
bufSamples :: Rate -> UGen -> UGen
bufSamples rate bufnum = mkUGen Nothing [InitialisationRate,ControlRate] (Left rate) "BufSamples" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer writing oscillator.
--
--  BufWr [ControlRate,AudioRate] bufnum=0 phase=0 loop=1 *inputArray=0;    MCE=1, FILTER: TRUE, REORDERS INPUTS: [3,0,1,2], ENUMERATION INPUTS: 2=Loop
bufWr :: UGen -> UGen -> Loop UGen -> UGen -> UGen
bufWr bufnum phase loop inputArray = mkUGen Nothing [ControlRate,AudioRate] (Right [3]) "BufWr" [bufnum,phase,(from_loop loop)] (Just [inputArray]) 1 (Special 0) NoId

-- | Chorusing wavetable oscillator.
--
--  COsc [ControlRate,AudioRate] bufnum=0 freq=440 beats=0.5
cOsc :: Rate -> UGen -> UGen -> UGen -> UGen
cOsc rate bufnum freq beats = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "COsc" [bufnum,freq,beats] Nothing 1 (Special 0) NoId

-- | Test for infinity, not-a-number, and denormals
--
--  CheckBadValues [ControlRate,AudioRate] in=0 id=0 post=2;    FILTER: TRUE
checkBadValues :: UGen -> UGen -> UGen -> UGen
checkBadValues in_ id_ post = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "CheckBadValues" [in_,id_,post] Nothing 1 (Special 0) NoId

-- | Clip a signal outside given thresholds.
--
--  Clip [InitialisationRate,ControlRate,AudioRate] in=0 lo=0 hi=1;    FILTER: TRUE
clip :: UGen -> UGen -> UGen -> UGen
clip in_ lo hi = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Right [0]) "Clip" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Clip Noise.
--
--  ClipNoise [ControlRate,AudioRate] ;    NONDET
clipNoiseId :: ID a => a -> Rate -> UGen
clipNoiseId z rate = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "ClipNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of ClipNoise.
clipNoiseM :: UId m => Rate -> m UGen
clipNoiseM = liftUId1 clipNoiseId

-- | Unsafe variant of ClipNoise.
clipNoise ::  Rate -> UGen
clipNoise = liftUnsafe1 clipNoiseM

-- | Statistical gate.
--
--  CoinGate [ControlRate,AudioRate] prob=0 in=0;    FILTER: TRUE, NONDET
coinGateId :: ID a => a -> UGen -> UGen -> UGen
coinGateId z prob in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "CoinGate" [prob,in_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of CoinGate.
coinGateM :: UId m => UGen -> UGen -> m UGen
coinGateM = liftUId2 coinGateId

-- | Unsafe variant of CoinGate.
coinGate ::  UGen -> UGen -> UGen
coinGate = liftUnsafe2 coinGateM

-- | Comb delay line with cubic interpolation.
--
--  CombC [ControlRate,AudioRate] in=0 maxdelaytime=0.2 delaytime=0.2 decaytime=1;    FILTER: TRUE
combC :: UGen -> UGen -> UGen -> UGen -> UGen
combC in_ maxdelaytime delaytime decaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "CombC" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Comb delay line with linear interpolation.
--
--  CombL [ControlRate,AudioRate] in=0 maxdelaytime=0.2 delaytime=0.2 decaytime=1;    FILTER: TRUE
combL :: UGen -> UGen -> UGen -> UGen -> UGen
combL in_ maxdelaytime delaytime decaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "CombL" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Comb delay line with no interpolation.
--
--  CombN [ControlRate,AudioRate] in=0 maxdelaytime=0.2 delaytime=0.2 decaytime=1;    FILTER: TRUE
combN :: UGen -> UGen -> UGen -> UGen -> UGen
combN in_ maxdelaytime delaytime decaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "CombN" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Compressor, expander, limiter, gate, ducker
--
--  Compander [AudioRate] in=0 control=0 thresh=0.5 slopeBelow=1 slopeAbove=1 clampTime=0.01 relaxTime=0.1;    FILTER: TRUE
compander :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
compander in_ control_ thresh slopeBelow slopeAbove clampTime relaxTime = mkUGen Nothing [AudioRate] (Right [0]) "Compander" [in_,control_,thresh,slopeBelow,slopeAbove,clampTime,relaxTime] Nothing 1 (Special 0) NoId

-- | Compressor, expander, limiter, gate, ducker.
--
--  CompanderD [AudioRate] in=0 thresh=0.5 slopeBelow=1 slopeAbove=1 clampTime=0.01 relaxTime=0.01
companderD :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
companderD rate in_ thresh slopeBelow slopeAbove clampTime relaxTime = mkUGen Nothing [AudioRate] (Left rate) "CompanderD" [in_,thresh,slopeBelow,slopeAbove,clampTime,relaxTime] Nothing 1 (Special 0) NoId

-- | Duration of one block
--
--  ControlDur [InitialisationRate]
controlDur :: UGen
controlDur = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "ControlDur" [] Nothing 1 (Special 0) NoId

-- | Server control rate.
--
--  ControlRate [InitialisationRate]
controlRate :: UGen
controlRate = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "ControlRate" [] Nothing 1 (Special 0) NoId

-- | Real-time convolver.
--
--  Convolution [AudioRate] in=0 kernel=0 framesize=512
convolution :: UGen -> UGen -> UGen -> UGen
convolution in_ kernel framesize = mkUGen Nothing [AudioRate] (Left AudioRate) "Convolution" [in_,kernel,framesize] Nothing 1 (Special 0) NoId

-- | Real-time fixed kernel convolver.
--
--  Convolution2 [AudioRate] in=0 kernel=0 trigger=0 framesize=2048
convolution2 :: UGen -> UGen -> UGen -> UGen -> UGen
convolution2 in_ kernel trigger framesize = mkUGen Nothing [AudioRate] (Left AudioRate) "Convolution2" [in_,kernel,trigger,framesize] Nothing 1 (Special 0) NoId

-- | Real-time convolver with linear interpolation
--
--  Convolution2L [AudioRate] in=0 kernel=0 trigger=0 framesize=2048 crossfade=1
convolution2L :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
convolution2L rate in_ kernel trigger framesize crossfade = mkUGen Nothing [AudioRate] (Left rate) "Convolution2L" [in_,kernel,trigger,framesize,crossfade] Nothing 1 (Special 0) NoId

-- | Time based convolver.
--
--  Convolution3 [ControlRate,AudioRate] in=0 kernel=0 trigger=0 framesize=2048
convolution3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
convolution3 rate in_ kernel trigger framesize = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Convolution3" [in_,kernel,trigger,framesize] Nothing 1 (Special 0) NoId

-- | Chaotic noise function.
--
--  Crackle [ControlRate,AudioRate] chaosParam=1.5
crackle :: Rate -> UGen -> UGen
crackle rate chaosParam = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Crackle" [chaosParam] Nothing 1 (Special 0) NoId

-- | Cusp map chaotic generator
--
--  CuspL [AudioRate] freq=22050 a=1 b=1.9 xi=0
cuspL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
cuspL rate freq a b xi = mkUGen Nothing [AudioRate] (Left rate) "CuspL" [freq,a,b,xi] Nothing 1 (Special 0) NoId

-- | Cusp map chaotic generator
--
--  CuspN [AudioRate] freq=22050 a=1 b=1.9 xi=0
cuspN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
cuspN rate freq a b xi = mkUGen Nothing [AudioRate] (Left rate) "CuspN" [freq,a,b,xi] Nothing 1 (Special 0) NoId

-- | Create a constant amplitude signal
--
--  DC [ControlRate,AudioRate] in=0
dc :: Rate -> UGen -> UGen
dc rate in_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "DC" [in_] Nothing 1 (Special 0) NoId

-- | Demand rate brownian movement generator.
--
--  Dbrown [DemandRate] length=100000000 lo=0 hi=1 step=0.01;    REORDERS INPUTS: [1,2,3,0], DEMAND/NONDET
dbrownId :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
dbrownId z length_ lo hi step = mkUGen Nothing [DemandRate] (Left DemandRate) "Dbrown" [length_,lo,hi,step] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dbrown.
dbrownM :: UId m => UGen -> UGen -> UGen -> UGen -> m UGen
dbrownM = liftUId4 dbrownId

-- | Unsafe variant of Dbrown.
dbrown ::  UGen -> UGen -> UGen -> UGen -> UGen
dbrown = liftUnsafe4 dbrownM

-- | Buffer read demand ugen
--
--  Dbufrd [DemandRate] bufnum=0 phase=0 loop=1;    ENUMERATION INPUTS: 2=Loop, DEMAND/NONDET
dbufrdId :: ID a => a -> UGen -> UGen -> Loop UGen -> UGen
dbufrdId z bufnum phase loop = mkUGen Nothing [DemandRate] (Left DemandRate) "Dbufrd" [bufnum,phase,(from_loop loop)] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dbufrd.
dbufrdM :: UId m => UGen -> UGen -> Loop UGen -> m UGen
dbufrdM = liftUId3 dbufrdId

-- | Unsafe variant of Dbufrd.
dbufrd ::  UGen -> UGen -> Loop UGen -> UGen
dbufrd = liftUnsafe3 dbufrdM

-- | Buffer write demand ugen
--
--  Dbufwr [DemandRate] bufnum=0 phase=0 input=0 loop=1;    REORDERS INPUTS: [2,0,1,3], ENUMERATION INPUTS: 3=Loop, DEMAND/NONDET
dbufwrId :: ID a => a -> UGen -> UGen -> UGen -> Loop UGen -> UGen
dbufwrId z bufnum phase input loop = mkUGen Nothing [DemandRate] (Left DemandRate) "Dbufwr" [bufnum,phase,input,(from_loop loop)] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dbufwr.
dbufwrM :: UId m => UGen -> UGen -> UGen -> Loop UGen -> m UGen
dbufwrM = liftUId4 dbufwrId

-- | Unsafe variant of Dbufwr.
dbufwr :: UGen -> UGen -> UGen -> Loop UGen -> UGen
dbufwr = liftUnsafe4 dbufwrM

-- | Constrain a demand-rate stream to a given sum
--
--  Dconst [DemandRate] sum=0 in=0 tolerance=0.001;    DEMAND/NONDET
dconstId :: ID a => a -> UGen -> UGen -> UGen -> UGen
dconstId z sum_ in_ tolerance = mkUGen Nothing [DemandRate] (Left DemandRate) "Dconst" [sum_,in_,tolerance] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dconst.
dconstM :: UId m => UGen -> UGen -> UGen -> m UGen
dconstM = liftUId3 dconstId

-- | Unsafe variant of Dconst.
dconst ::  UGen -> UGen -> UGen -> UGen
dconst = liftUnsafe3 dconstM

-- | Exponential decay
--
--  Decay [ControlRate,AudioRate] in=0 decayTime=1;    FILTER: TRUE
decay :: UGen -> UGen -> UGen
decay in_ decayTime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Decay" [in_,decayTime] Nothing 1 (Special 0) NoId

-- | Exponential decay
--
--  Decay2 [ControlRate,AudioRate] in=0 attackTime=0.01 decayTime=1;    FILTER: TRUE
decay2 :: UGen -> UGen -> UGen -> UGen
decay2 in_ attackTime decayTime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Decay2" [in_,attackTime,decayTime] Nothing 1 (Special 0) NoId

-- | 2D Ambisonic B-format decoder.
--
--  DecodeB2 [ControlRate,AudioRate] w=0 x=0 y=0 orientation=0.5;    NC INPUT: True, FILTER: TRUE
decodeB2 :: Int -> UGen -> UGen -> UGen -> UGen -> UGen
decodeB2 numChannels w x y orientation = mkUGen Nothing [ControlRate,AudioRate] (Right [0,1,2]) "DecodeB2" [w,x,y,orientation] Nothing numChannels (Special 0) NoId

-- | Convert signal to modal pitch.
--
--  DegreeToKey [ControlRate,AudioRate] bufnum=0 in=0 octave=12;    FILTER: TRUE
degreeToKey :: UGen -> UGen -> UGen -> UGen
degreeToKey bufnum in_ octave = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "DegreeToKey" [bufnum,in_,octave] Nothing 1 (Special 0) NoId

-- | Tap a delay line from a DelTapWr UGen
--
--  DelTapRd [ControlRate,AudioRate] buffer=0 phase=0 delTime=0 interp=1;    FILTER: TRUE
delTapRd :: UGen -> UGen -> UGen -> UGen -> UGen
delTapRd buffer phase delTime interp = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "DelTapRd" [buffer,phase,delTime,interp] Nothing 1 (Special 0) NoId

-- | Write to a buffer for a DelTapRd UGen
--
--  DelTapWr [ControlRate,AudioRate] buffer=0 in=0;    FILTER: TRUE
delTapWr :: UGen -> UGen -> UGen
delTapWr buffer in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "DelTapWr" [buffer,in_] Nothing 1 (Special 0) NoId

-- | Single sample delay.
--
--  Delay1 [ControlRate,AudioRate] in=0;    FILTER: TRUE
delay1 :: UGen -> UGen
delay1 in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Delay1" [in_] Nothing 1 (Special 0) NoId

-- | Two sample delay.
--
--  Delay2 [ControlRate,AudioRate] in=0;    FILTER: TRUE
delay2 :: UGen -> UGen
delay2 in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Delay2" [in_] Nothing 1 (Special 0) NoId

-- | Simple delay line with cubic interpolation.
--
--  DelayC [ControlRate,AudioRate] in=0 maxdelaytime=0.2 delaytime=0.2;    FILTER: TRUE
delayC :: UGen -> UGen -> UGen -> UGen
delayC in_ maxdelaytime delaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "DelayC" [in_,maxdelaytime,delaytime] Nothing 1 (Special 0) NoId

-- | Simple delay line with linear interpolation.
--
--  DelayL [ControlRate,AudioRate] in=0 maxdelaytime=0.2 delaytime=0.2;    FILTER: TRUE
delayL :: UGen -> UGen -> UGen -> UGen
delayL in_ maxdelaytime delaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "DelayL" [in_,maxdelaytime,delaytime] Nothing 1 (Special 0) NoId

-- | Simple delay line with no interpolation.
--
--  DelayN [ControlRate,AudioRate] in=0 maxdelaytime=0.2 delaytime=0.2;    FILTER: TRUE
delayN :: UGen -> UGen -> UGen -> UGen
delayN in_ maxdelaytime delaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "DelayN" [in_,maxdelaytime,delaytime] Nothing 1 (Special 0) NoId

-- | Demand results from demand rate UGens.
--
--  Demand [ControlRate,AudioRate] trig=0 reset=0 *demandUGens=0;    MCE=1, FILTER: TRUE
demand :: UGen -> UGen -> UGen -> UGen
demand trig_ reset demandUGens = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Demand" [trig_,reset] (Just [demandUGens]) (length (mceChannels demandUGens) + 0) (Special 0) NoId

-- | Demand rate envelope generator
--
--  DemandEnvGen [ControlRate,AudioRate] level=0 dur=0 shape=1 curve=0 gate=1 reset=1 levelScale=1 levelBias=0 timeScale=1 doneAction=0;    ENUMERATION INPUTS: 9=DoneAction
demandEnvGen :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> DoneAction UGen -> UGen
demandEnvGen rate level dur shape curve gate_ reset levelScale levelBias timeScale doneAction = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "DemandEnvGen" [level,dur,shape,curve,gate_,reset,levelScale,levelBias,timeScale,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Search a buffer for a value
--
--  DetectIndex [ControlRate,AudioRate] bufnum=0 in=0;    FILTER: TRUE
detectIndex :: UGen -> UGen -> UGen
detectIndex bufnum in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "DetectIndex" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Detect when input falls below an amplitude threshold
--
--  DetectSilence [ControlRate,AudioRate] in=0 amp=0.0001 time=0.1 doneAction=0;    FILTER: TRUE, ENUMERATION INPUTS: 3=DoneAction
detectSilence :: UGen -> UGen -> UGen -> DoneAction UGen -> UGen
detectSilence in_ amp time doneAction = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "DetectSilence" [in_,amp,time,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Demand rate geometric series UGen.
--
--  Dgeom [DemandRate] length=100000000 start=1 grow=2;    REORDERS INPUTS: [1,2,0], DEMAND/NONDET
dgeomId :: ID a => a -> UGen -> UGen -> UGen -> UGen
dgeomId z length_ start grow = mkUGen Nothing [DemandRate] (Left DemandRate) "Dgeom" [length_,start,grow] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dgeom.
dgeomM :: UId m => UGen -> UGen -> UGen -> m UGen
dgeomM = liftUId3 dgeomId

-- | Unsafe variant of Dgeom.
dgeom ::  UGen -> UGen -> UGen -> UGen
dgeom = liftUnsafe3 dgeomM

-- | Demand rate brownian movement generator.
--
--  Dibrown [DemandRate] length=100000000 lo=0 hi=1 step=0.01;    REORDERS INPUTS: [1,2,3,0], DEMAND/NONDET
dibrownId :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
dibrownId z length_ lo hi step = mkUGen Nothing [DemandRate] (Left DemandRate) "Dibrown" [length_,lo,hi,step] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dibrown.
dibrownM :: UId m => UGen -> UGen -> UGen -> UGen -> m UGen
dibrownM = liftUId4 dibrownId

-- | Unsafe variant of Dibrown.
dibrown ::  UGen -> UGen -> UGen -> UGen -> UGen
dibrown = liftUnsafe4 dibrownM

-- | Stream in audio from a file.
--
--  DiskIn [AudioRate] bufnum=0 loop=0;    NC INPUT: True, ENUMERATION INPUTS: 1=Loop
diskIn :: Int -> UGen -> Loop UGen -> UGen
diskIn numChannels bufnum loop = mkUGen Nothing [AudioRate] (Left AudioRate) "DiskIn" [bufnum,(from_loop loop)] Nothing numChannels (Special 0) NoId

-- | Record to a soundfile to disk.
--
--  DiskOut [AudioRate] bufnum=0 *channelsArray=0;    MCE=1
diskOut :: UGen -> UGen -> UGen
diskOut bufnum input = mkUGen Nothing [AudioRate] (Left AudioRate) "DiskOut" [bufnum] (Just [input]) 1 (Special 0) NoId

-- | Demand rate white noise random generator.
--
--  Diwhite [DemandRate] length=100000000 lo=0 hi=1;    REORDERS INPUTS: [1,2,0], DEMAND/NONDET
diwhiteId :: ID a => a -> UGen -> UGen -> UGen -> UGen
diwhiteId z length_ lo hi = mkUGen Nothing [DemandRate] (Left DemandRate) "Diwhite" [length_,lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Diwhite.
diwhiteM :: UId m => UGen -> UGen -> UGen -> m UGen
diwhiteM = liftUId3 diwhiteId

-- | Unsafe variant of Diwhite.
diwhite ::  UGen -> UGen -> UGen -> UGen
diwhite = liftUnsafe3 diwhiteM

-- | Monitors another UGen to see when it is finished
--
--  Done [ControlRate] src=0
done :: UGen -> UGen
done src = mkUGen Nothing [ControlRate] (Left ControlRate) "Done" [src] Nothing 1 (Special 0) NoId

-- | Print the current output value of a demand rate UGen
--
--  Dpoll [DemandRate] in=0 label=0 run=1 trigid=-1;    DEMAND/NONDET
dpollId :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
dpollId z in_ label_ run trigid = mkUGen Nothing [DemandRate] (Left DemandRate) "Dpoll" [in_,label_,run,trigid] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dpoll.
dpollM :: UId m => UGen -> UGen -> UGen -> UGen -> m UGen
dpollM = liftUId4 dpollId

-- | Unsafe variant of Dpoll.
dpoll ::  UGen -> UGen -> UGen -> UGen -> UGen
dpoll = liftUnsafe4 dpollM

-- | Demand rate random sequence generator.
--
--  Drand [DemandRate] repeats=1 *list=0;    MCE=1, REORDERS INPUTS: [1,0], DEMAND/NONDET
drandId :: ID a => a -> UGen -> UGen -> UGen
drandId z repeats list_ = mkUGen Nothing [DemandRate] (Left DemandRate) "Drand" [repeats] (Just [list_]) 1 (Special 0) (toUId z)

-- | Monad variant of Drand.
drandM :: UId m => UGen -> UGen -> m UGen
drandM = liftUId2 drandId

-- | Unsafe variant of Drand.
drand ::  UGen -> UGen -> UGen
drand = liftUnsafe2 drandM

-- | demand rate reset
--
--  Dreset [DemandRate] in=0 reset=0;    DEMAND/NONDET
dresetId :: ID a => a -> UGen -> UGen -> UGen
dresetId z in_ reset = mkUGen Nothing [DemandRate] (Left DemandRate) "Dreset" [in_,reset] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dreset.
dresetM :: UId m => UGen -> UGen -> m UGen
dresetM = liftUId2 dresetId

-- | Unsafe variant of Dreset.
dreset ::  UGen -> UGen -> UGen
dreset = liftUnsafe2 dresetM

-- | Demand rate sequence generator.
--
--  Dseq [DemandRate] repeats=1 *list=0;    MCE=1, REORDERS INPUTS: [1,0], DEMAND/NONDET
dseqId :: ID a => a -> UGen -> UGen -> UGen
dseqId z repeats list_ = mkUGen Nothing [DemandRate] (Left DemandRate) "Dseq" [repeats] (Just [list_]) 1 (Special 0) (toUId z)

-- | Monad variant of Dseq.
dseqM :: UId m => UGen -> UGen -> m UGen
dseqM = liftUId2 dseqId

-- | Unsafe variant of Dseq.
dseq ::  UGen -> UGen -> UGen
dseq = liftUnsafe2 dseqM

-- | Demand rate sequence generator.
--
--  Dser [DemandRate] repeats=1 *list=0;    MCE=1, REORDERS INPUTS: [1,0], DEMAND/NONDET
dserId :: ID a => a -> UGen -> UGen -> UGen
dserId z repeats list_ = mkUGen Nothing [DemandRate] (Left DemandRate) "Dser" [repeats] (Just [list_]) 1 (Special 0) (toUId z)

-- | Monad variant of Dser.
dserM :: UId m => UGen -> UGen -> m UGen
dserM = liftUId2 dserId

-- | Unsafe variant of Dser.
dser ::  UGen -> UGen -> UGen
dser = liftUnsafe2 dserM

-- | Demand rate arithmetic series UGen.
--
--  Dseries [DemandRate] length=100000000 start=1 step=1;    REORDERS INPUTS: [1,2,0], DEMAND/NONDET
dseriesId :: ID a => a -> UGen -> UGen -> UGen -> UGen
dseriesId z length_ start step = mkUGen Nothing [DemandRate] (Left DemandRate) "Dseries" [length_,start,step] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dseries.
dseriesM :: UId m => UGen -> UGen -> UGen -> m UGen
dseriesM = liftUId3 dseriesId

-- | Unsafe variant of Dseries.
dseries ::  UGen -> UGen -> UGen -> UGen
dseries = liftUnsafe3 dseriesM

-- | Demand rate random sequence generator
--
--  Dshuf [DemandRate] repeats=1 *list=0;    MCE=1, REORDERS INPUTS: [1,0], DEMAND/NONDET
dshufId :: ID a => a -> UGen -> UGen -> UGen
dshufId z repeats list_ = mkUGen Nothing [DemandRate] (Left DemandRate) "Dshuf" [repeats] (Just [list_]) 1 (Special 0) (toUId z)

-- | Monad variant of Dshuf.
dshufM :: UId m => UGen -> UGen -> m UGen
dshufM = liftUId2 dshufId

-- | Unsafe variant of Dshuf.
dshuf ::  UGen -> UGen -> UGen
dshuf = liftUnsafe2 dshufM

-- | Demand rate input replicator
--
--  Dstutter [DemandRate] n=0 in=0;    DEMAND/NONDET
dstutterId :: ID a => a -> UGen -> UGen -> UGen
dstutterId z n in_ = mkUGen Nothing [DemandRate] (Left DemandRate) "Dstutter" [n,in_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dstutter.
dstutterM :: UId m => UGen -> UGen -> m UGen
dstutterM = liftUId2 dstutterId

-- | Unsafe variant of Dstutter.
dstutter ::  UGen -> UGen -> UGen
dstutter = liftUnsafe2 dstutterM

-- | Demand rate generator for embedding different inputs
--
--  Dswitch [DemandRate] index=0 *list=0;    MCE=1, REORDERS INPUTS: [1,0], DEMAND/NONDET
dswitchId :: ID a => a -> UGen -> UGen -> UGen
dswitchId z index_ list_ = mkUGen Nothing [DemandRate] (Left DemandRate) "Dswitch" [index_] (Just [list_]) 1 (Special 0) (toUId z)

-- | Monad variant of Dswitch.
dswitchM :: UId m => UGen -> UGen -> m UGen
dswitchM = liftUId2 dswitchId

-- | Unsafe variant of Dswitch.
dswitch ::  UGen -> UGen -> UGen
dswitch = liftUnsafe2 dswitchM

-- | Demand rate generator for switching between inputs.
--
--  Dswitch1 [DemandRate] index=0 *list=0;    MCE=1, REORDERS INPUTS: [1,0], DEMAND/NONDET
dswitch1Id :: ID a => a -> UGen -> UGen -> UGen
dswitch1Id z index_ list_ = mkUGen Nothing [DemandRate] (Left DemandRate) "Dswitch1" [index_] (Just [list_]) 1 (Special 0) (toUId z)

-- | Monad variant of Dswitch1.
dswitch1M :: UId m => UGen -> UGen -> m UGen
dswitch1M = liftUId2 dswitch1Id

-- | Unsafe variant of Dswitch1.
dswitch1 ::  UGen -> UGen -> UGen
dswitch1 = liftUnsafe2 dswitch1M

-- | Return the same unique series of values for several demand streams
--
--  Dunique [DemandRate] source=0 maxBufferSize=1024 protected=1;    DEMAND/NONDET
duniqueId :: ID a => a -> UGen -> UGen -> UGen -> UGen
duniqueId z source maxBufferSize protected = mkUGen Nothing [DemandRate] (Left DemandRate) "Dunique" [source,maxBufferSize,protected] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dunique.
duniqueM :: UId m => UGen -> UGen -> UGen -> m UGen
duniqueM = liftUId3 duniqueId

-- | Unsafe variant of Dunique.
dunique ::  UGen -> UGen -> UGen -> UGen
dunique = liftUnsafe3 duniqueM

-- | Random impulses.
--
--  Dust [ControlRate,AudioRate] density=0;    NONDET
dustId :: ID a => a -> Rate -> UGen -> UGen
dustId z rate density = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Dust" [density] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dust.
dustM :: UId m => Rate -> UGen -> m UGen
dustM = liftUId2 dustId

-- | Unsafe variant of Dust.
dust ::  Rate -> UGen -> UGen
dust = liftUnsafe2 dustM

-- | Random impulses.
--
--  Dust2 [ControlRate,AudioRate] density=0;    NONDET
dust2Id :: ID a => a -> Rate -> UGen -> UGen
dust2Id z rate density = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Dust2" [density] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dust2.
dust2M :: UId m => Rate -> UGen -> m UGen
dust2M = liftUId2 dust2Id

-- | Unsafe variant of Dust2.
dust2 ::  Rate -> UGen -> UGen
dust2 = liftUnsafe2 dust2M

-- | Demand results from demand rate UGens.
--
--  Duty [ControlRate,AudioRate] dur=1 reset=0 doneAction=0 level=1;    REORDERS INPUTS: [0,1,3,2], ENUMERATION INPUTS: 2=DoneAction
duty :: Rate -> UGen -> UGen -> DoneAction UGen -> UGen -> UGen
duty rate dur reset doneAction level = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Duty" [dur,reset,(from_done_action doneAction),level] Nothing 1 (Special 0) NoId

-- | Demand rate white noise random generator.
--
--  Dwhite [DemandRate] length=100000000 lo=0 hi=1;    REORDERS INPUTS: [1,2,0], DEMAND/NONDET
dwhiteId :: ID a => a -> UGen -> UGen -> UGen -> UGen
dwhiteId z length_ lo hi = mkUGen Nothing [DemandRate] (Left DemandRate) "Dwhite" [length_,lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dwhite.
dwhiteM :: UId m => UGen -> UGen -> UGen -> m UGen
dwhiteM = liftUId3 dwhiteId

-- | Unsafe variant of Dwhite.
dwhite ::  UGen -> UGen -> UGen -> UGen
dwhite = liftUnsafe3 dwhiteM

{-
-- | Demand rate weighted random sequence generator
--
--  Dwrand [DemandRate] repeats=1 weights=0 *list=0;    MCE=1, REORDERS INPUTS: [2,1,0], DEMAND/NONDET
dwrandId :: ID a => a -> UGen -> UGen -> UGen -> UGen
dwrandId z repeats weights list_ = mkUGen Nothing [DemandRate] (Left DemandRate) "Dwrand" [repeats,weights] (Just [list_]) 1 (Special 0) (toUId z)

-- | Monad variant of Dwrand.
dwrandM :: UId m => UGen -> UGen -> UGen -> m UGen
dwrandM = liftUId3 dwrandId

-- | Unsafe variant of Dwrand.
dwrand ::  UGen -> UGen -> UGen -> UGen
dwrand = liftUnsafe3 dwrandM
-}

-- | Demand rate random sequence generator.
--
--  Dxrand [DemandRate] repeats=1 *list=0;    MCE=1, REORDERS INPUTS: [1,0], DEMAND/NONDET
dxrandId :: ID a => a -> UGen -> UGen -> UGen
dxrandId z repeats list_ = mkUGen Nothing [DemandRate] (Left DemandRate) "Dxrand" [repeats] (Just [list_]) 1 (Special 0) (toUId z)

-- | Monad variant of Dxrand.
dxrandM :: UId m => UGen -> UGen -> m UGen
dxrandM = liftUId2 dxrandId

-- | Unsafe variant of Dxrand.
dxrand ::  UGen -> UGen -> UGen
dxrand = liftUnsafe2 dxrandM

-- | Envelope generator
--
--  EnvGen [ControlRate,AudioRate] gate=1 levelScale=1 levelBias=0 timeScale=1 doneAction=0 *envelope=0;    MCE=1, REORDERS INPUTS: [5,0,1,2,3,4], ENUMERATION INPUTS: 4=DoneAction, 5=Envelope
envGen :: Rate -> UGen -> UGen -> UGen -> UGen -> DoneAction UGen -> Envelope UGen -> UGen
envGen rate gate_ levelScale levelBias timeScale doneAction envelope_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "EnvGen" [gate_,levelScale,levelBias,timeScale,(from_done_action doneAction)] (Just [(envelope_to_ugen envelope_)]) 1 (Special 0) NoId

-- | Exponential single random number generator.
--
--  ExpRand [InitialisationRate] lo=0.01 hi=1;    NONDET
expRandId :: ID a => a -> UGen -> UGen -> UGen
expRandId z lo hi = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "ExpRand" [lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of ExpRand.
expRandM :: UId m => UGen -> UGen -> m UGen
expRandM = liftUId2 expRandId

-- | Unsafe variant of ExpRand.
expRand ::  UGen -> UGen -> UGen
expRand = liftUnsafe2 expRandM

-- | Feedback sine with chaotic phase indexing
--
--  FBSineC [AudioRate] freq=22050 im=1 fb=0.1 a=1.1 c=0.5 xi=0.1 yi=0.1
fbSineC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fbSineC rate freq im fb a c xi yi = mkUGen Nothing [AudioRate] (Left rate) "FBSineC" [freq,im,fb,a,c,xi,yi] Nothing 1 (Special 0) NoId

-- | Feedback sine with chaotic phase indexing
--
--  FBSineL [AudioRate] freq=22050 im=1 fb=0.1 a=1.1 c=0.5 xi=0.1 yi=0.1
fbSineL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fbSineL rate freq im fb a c xi yi = mkUGen Nothing [AudioRate] (Left rate) "FBSineL" [freq,im,fb,a,c,xi,yi] Nothing 1 (Special 0) NoId

-- | Feedback sine with chaotic phase indexing
--
--  FBSineN [AudioRate] freq=22050 im=1 fb=0.1 a=1.1 c=0.5 xi=0.1 yi=0.1
fbSineN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fbSineN rate freq im fb a c xi yi = mkUGen Nothing [AudioRate] (Left rate) "FBSineN" [freq,im,fb,a,c,xi,yi] Nothing 1 (Special 0) NoId

-- | Fast Fourier Transform
--
--  FFT [ControlRate] buffer=0 in=0 hop=0.5 wintype=0 active=1 winsize=0
fft :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fft buffer in_ hop wintype active winsize = mkUGen Nothing [ControlRate] (Left ControlRate) "FFT" [buffer,in_,hop,wintype,active,winsize] Nothing 1 (Special 0) NoId

-- | First order filter section.
--
--  FOS [ControlRate,AudioRate] in=0 a0=0 a1=0 b1=0;    FILTER: TRUE
fos :: UGen -> UGen -> UGen -> UGen -> UGen
fos in_ a0 a1 b1 = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "FOS" [in_,a0,a1,b1] Nothing 1 (Special 0) NoId

-- | Fast sine oscillator.
--
--  FSinOsc [ControlRate,AudioRate] freq=440 iphase=0
fSinOsc :: Rate -> UGen -> UGen -> UGen
fSinOsc rate freq iphase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "FSinOsc" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Fold a signal outside given thresholds.
--
--  Fold [InitialisationRate,ControlRate,AudioRate] in=0 lo=0 hi=1;    FILTER: TRUE
fold :: UGen -> UGen -> UGen -> UGen
fold in_ lo hi = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Right [0]) "Fold" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Formant oscillator
--
--  Formant [AudioRate] fundfreq=440 formfreq=1760 bwfreq=880
formant :: Rate -> UGen -> UGen -> UGen -> UGen
formant rate fundfreq formfreq bwfreq = mkUGen Nothing [AudioRate] (Left rate) "Formant" [fundfreq,formfreq,bwfreq] Nothing 1 (Special 0) NoId

-- | FOF-like filter.
--
--  Formlet [ControlRate,AudioRate] in=0 freq=440 attacktime=1 decaytime=1;    FILTER: TRUE
formlet :: UGen -> UGen -> UGen -> UGen -> UGen
formlet in_ freq attacktime decaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Formlet" [in_,freq,attacktime,decaytime] Nothing 1 (Special 0) NoId

-- | When triggered, frees a node.
--
--  Free [ControlRate] trig=0 id=0;    FILTER: TRUE
free :: UGen -> UGen -> UGen
free trig_ id_ = mkUGen Nothing [ControlRate] (Right [0]) "Free" [trig_,id_] Nothing 1 (Special 0) NoId

-- | When triggered, free enclosing synth.
--
--  FreeSelf [ControlRate] in=0
freeSelf :: UGen -> UGen
freeSelf in_ = mkUGen Nothing [ControlRate] (Left ControlRate) "FreeSelf" [in_] Nothing 1 (Special 0) NoId

-- | Free the enclosing synth when a UGen is finished
--
--  FreeSelfWhenDone [ControlRate] src=0
freeSelfWhenDone :: UGen -> UGen
freeSelfWhenDone src = mkUGen Nothing [ControlRate] (Left ControlRate) "FreeSelfWhenDone" [src] Nothing 1 (Special 0) NoId

-- | A reverb
--
--  FreeVerb [AudioRate] in=0 mix=0.33 room=0.5 damp=0.5;    FILTER: TRUE
freeVerb :: UGen -> UGen -> UGen -> UGen -> UGen
freeVerb in_ mix room damp = mkUGen Nothing [AudioRate] (Right [0]) "FreeVerb" [in_,mix,room,damp] Nothing 1 (Special 0) NoId

-- | A two-channel reverb
--
--  FreeVerb2 [AudioRate] in=0 in2=0 mix=0.33 room=0.5 damp=0.5;    FILTER: TRUE
freeVerb2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
freeVerb2 in_ in2 mix room damp = mkUGen Nothing [AudioRate] (Right [0]) "FreeVerb2" [in_,in2,mix,room,damp] Nothing 2 (Special 0) NoId

-- | Frequency Shifter.
--
--  FreqShift [AudioRate] in=0 freq=0 phase=0
freqShift :: UGen -> UGen -> UGen -> UGen
freqShift in_ freq phase = mkUGen Nothing [AudioRate] (Left AudioRate) "FreqShift" [in_,freq,phase] Nothing 1 (Special 0) NoId

-- | A two-channel reverb
--
--  GVerb [AudioRate] in=0 roomsize=10 revtime=3 damping=0.5 inputbw=0.5 spread=15 drylevel=1 earlyreflevel=0.7 taillevel=0.5 maxroomsize=300;    FILTER: TRUE
gVerb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gVerb in_ roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize = mkUGen Nothing [AudioRate] (Right [0]) "GVerb" [in_,roomsize,revtime,damping,inputbw,spread,drylevel,earlyreflevel,taillevel,maxroomsize] Nothing 2 (Special 0) NoId

-- | Gate or hold.
--
--  Gate [ControlRate,AudioRate] in=0 trig=0;    FILTER: TRUE
gate :: UGen -> UGen -> UGen
gate in_ trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Gate" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Gingerbreadman map chaotic generator
--
--  GbmanL [AudioRate] freq=22050 xi=1.2 yi=2.1
gbmanL :: Rate -> UGen -> UGen -> UGen -> UGen
gbmanL rate freq xi yi = mkUGen Nothing [AudioRate] (Left rate) "GbmanL" [freq,xi,yi] Nothing 1 (Special 0) NoId

-- | Gingerbreadman map chaotic generator
--
--  GbmanN [AudioRate] freq=22050 xi=1.2 yi=2.1
gbmanN :: Rate -> UGen -> UGen -> UGen -> UGen
gbmanN rate freq xi yi = mkUGen Nothing [AudioRate] (Left rate) "GbmanN" [freq,xi,yi] Nothing 1 (Special 0) NoId

-- | Dynamic stochastic synthesis generator.
--
--  Gendy1 [ControlRate,AudioRate] ampdist=1 durdist=1 adparam=1 ddparam=1 minfreq=440 maxfreq=660 ampscale=0.5 durscale=0.5 initCPs=12 knum=0;    NONDET
gendy1Id :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy1Id z rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Gendy1" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Gendy1.
gendy1M :: UId m => Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> m UGen
gendy1M = liftUId11 gendy1Id

-- | Unsafe variant of Gendy1.
gendy1 ::  Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy1 = liftUnsafe11 gendy1M

-- | Dynamic stochastic synthesis generator.
--
--  Gendy2 [ControlRate,AudioRate] ampdist=1 durdist=1 adparam=1 ddparam=1 minfreq=440 maxfreq=660 ampscale=0.5 durscale=0.5 initCPs=12 knum=0 a=1.17 c=0.31;    NONDET
gendy2Id :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy2Id z rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Gendy2" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum,a,c] Nothing 1 (Special 0) (toUId z)

{-
-- | Monad variant of Gendy2.
gendy2M :: UId m => Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> m UGen
gendy2M = liftUId13 gendy2Id

-- | Unsafe variant of Gendy2.
gendy2 ::  Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy2 = liftUnsafe13 gendy2M
-}
-- | Dynamic stochastic synthesis generator.
--
--  Gendy3 [ControlRate,AudioRate] ampdist=1 durdist=1 adparam=1 ddparam=1 freq=440 ampscale=0.5 durscale=0.5 initCPs=12 knum=0;    NONDET
gendy3Id :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy3Id z rate ampdist durdist adparam ddparam freq ampscale durscale initCPs knum = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Gendy3" [ampdist,durdist,adparam,ddparam,freq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Gendy3.
gendy3M :: UId m => Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> m UGen
gendy3M = liftUId10 gendy3Id

-- | Unsafe variant of Gendy3.
gendy3 ::  Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy3 = liftUnsafe10 gendy3M

-- | Granular synthesis with sound stored in a buffer
--
--  GrainBuf [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 interp=2 pan=0 envbufnum=-1 maxGrains=512;    NC INPUT: True
grainBuf :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainBuf numChannels trigger dur sndbuf rate_ pos interp pan envbufnum maxGrains = mkUGen Nothing [AudioRate] (Left AudioRate) "GrainBuf" [trigger,dur,sndbuf,rate_,pos,interp,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Granular synthesis with frequency modulated sine tones
--
--  GrainFM [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 pan=0 envbufnum=-1 maxGrains=512;    NC INPUT: True
grainFM :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainFM numChannels trigger dur carfreq modfreq index_ pan envbufnum maxGrains = mkUGen Nothing [AudioRate] (Left AudioRate) "GrainFM" [trigger,dur,carfreq,modfreq,index_,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Granulate an input signal
--
--  GrainIn [AudioRate] trigger=0 dur=1 in=0 pan=0 envbufnum=-1 maxGrains=512;    NC INPUT: True
grainIn :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainIn numChannels trigger dur in_ pan envbufnum maxGrains = mkUGen Nothing [AudioRate] (Left AudioRate) "GrainIn" [trigger,dur,in_,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Granular synthesis with sine tones
--
--  GrainSin [AudioRate] trigger=0 dur=1 freq=440 pan=0 envbufnum=-1 maxGrains=512;    NC INPUT: True
grainSin :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainSin numChannels trigger dur freq pan envbufnum maxGrains = mkUGen Nothing [AudioRate] (Left AudioRate) "GrainSin" [trigger,dur,freq,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Gray Noise.
--
--  GrayNoise [ControlRate,AudioRate] ;    NONDET
grayNoiseId :: ID a => a -> Rate -> UGen
grayNoiseId z rate = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "GrayNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of GrayNoise.
grayNoiseM :: UId m => Rate -> m UGen
grayNoiseM = liftUId1 grayNoiseId

-- | Unsafe variant of GrayNoise.
grayNoise ::  Rate -> UGen
grayNoise = liftUnsafe1 grayNoiseM

-- | 2nd order Butterworth highpass filter.
--
--  HPF [ControlRate,AudioRate] in=0 freq=440;    FILTER: TRUE
hpf :: UGen -> UGen -> UGen
hpf in_ freq = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "HPF" [in_,freq] Nothing 1 (Special 0) NoId

-- | Two point difference filter
--
--  HPZ1 [ControlRate,AudioRate] in=0;    FILTER: TRUE
hpz1 :: UGen -> UGen
hpz1 in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "HPZ1" [in_] Nothing 1 (Special 0) NoId

-- | Two zero fixed midcut.
--
--  HPZ2 [ControlRate,AudioRate] in=0;    FILTER: TRUE
hpz2 :: UGen -> UGen
hpz2 in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "HPZ2" [in_] Nothing 1 (Special 0) NoId

-- | Scrambled value with a hash function.
--
--  Hasher [ControlRate,AudioRate] in=0;    FILTER: TRUE
hasher :: UGen -> UGen
hasher in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Hasher" [in_] Nothing 1 (Special 0) NoId

-- | Henon map chaotic generator
--
--  HenonC [AudioRate] freq=22050 a=1.4 b=0.3 x0=0 x1=0
henonC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonC rate freq a b x0 x1 = mkUGen Nothing [AudioRate] (Left rate) "HenonC" [freq,a,b,x0,x1] Nothing 1 (Special 0) NoId

-- | Henon map chaotic generator
--
--  HenonL [AudioRate] freq=22050 a=1.4 b=0.3 x0=0 x1=0
henonL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonL rate freq a b x0 x1 = mkUGen Nothing [AudioRate] (Left rate) "HenonL" [freq,a,b,x0,x1] Nothing 1 (Special 0) NoId

-- | Henon map chaotic generator
--
--  HenonN [AudioRate] freq=22050 a=1.4 b=0.3 x0=0 x1=0
henonN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonN rate freq a b x0 x1 = mkUGen Nothing [AudioRate] (Left rate) "HenonN" [freq,a,b,x0,x1] Nothing 1 (Special 0) NoId

-- | Applies the Hilbert transform to an input signal.
--
--  Hilbert [AudioRate] in=0;    FILTER: TRUE
hilbert :: UGen -> UGen
hilbert in_ = mkUGen Nothing [AudioRate] (Right [0]) "Hilbert" [in_] Nothing 2 (Special 0) NoId

-- | Envelope generator for polling values from an Env
--
--  IEnvGen [ControlRate,AudioRate] index=0 *envelope=0;    MCE=1, REORDERS INPUTS: [1,0], ENUMERATION INPUTS: 1=IEnvelope
iEnvGen :: Rate -> UGen -> Envelope UGen -> UGen
iEnvGen rate index_ envelope_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "IEnvGen" [index_] (Just [(envelope_to_ienvgen_ugen envelope_)]) 1 (Special 0) NoId

-- | Inverse Fast Fourier Transform
--
--  IFFT [ControlRate,AudioRate] buffer=0 wintype=0 winsize=0
ifft :: UGen -> UGen -> UGen -> UGen
ifft buffer wintype winsize = mkUGen Nothing [ControlRate,AudioRate] (Left AudioRate) "IFFT" [buffer,wintype,winsize] Nothing 1 (Special 0) NoId

-- | Single integer random number generator.
--
--  iRand [InitialisationRate] lo=0 hi=127;    NONDET
iRandId :: ID a => a -> UGen -> UGen -> UGen
iRandId z lo hi = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "IRand" [lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of irand.
iRandM :: UId m => UGen -> UGen -> m UGen
iRandM = liftUId2 iRandId

-- | Unsafe variant of irand.
iRand ::  UGen -> UGen -> UGen
iRand = liftUnsafe2 iRandM

-- | Impulse oscillator.
--
--  Impulse [ControlRate,AudioRate] freq=440 phase=0
impulse :: Rate -> UGen -> UGen -> UGen
impulse rate freq phase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Impulse" [freq,phase] Nothing 1 (Special 0) NoId

-- | Read a signal from a bus.
--
--  In [ControlRate,AudioRate] bus=0;    NC INPUT: True
in' :: Int -> Rate -> UGen -> UGen
in' numChannels rate bus = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "In" [bus] Nothing numChannels (Special 0) NoId

-- | Read signal from a bus with a current or one cycle old timestamp.
--
--  InFeedback [AudioRate] bus=0;    NC INPUT: True
inFeedback :: Int -> UGen -> UGen
inFeedback numChannels bus = mkUGen Nothing [AudioRate] (Left AudioRate) "InFeedback" [bus] Nothing numChannels (Special 0) NoId

-- | Tests if a signal is within a given range.
--
--  InRange [InitialisationRate,ControlRate,AudioRate] in=0 lo=0 hi=1;    FILTER: TRUE
inRange :: UGen -> UGen -> UGen -> UGen
inRange in_ lo hi = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Right [0]) "InRange" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Test if a point is within a given rectangle.
--
--  InRect [ControlRate,AudioRate] x=0 y=0 rect=0
inRect :: Rate -> UGen -> UGen -> UGen -> UGen
inRect rate x y rect = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "InRect" [x,y,rect] Nothing 1 (Special 0) NoId

-- | Generate a trigger anytime a bus is set.
--
--  InTrig [ControlRate] bus=0;    NC INPUT: True
inTrig :: Int -> UGen -> UGen
inTrig numChannels bus = mkUGen Nothing [ControlRate] (Left ControlRate) "InTrig" [bus] Nothing numChannels (Special 0) NoId

-- | Index into a table with a signal
--
--  Index [ControlRate,AudioRate] bufnum=0 in=0;    FILTER: TRUE
index :: UGen -> UGen -> UGen
index bufnum in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "Index" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Finds the (lowest) point in the Buffer at which the input signal lies in-between the two values
--
--  IndexInBetween [ControlRate,AudioRate] bufnum=0 in=0;    FILTER: TRUE
indexInBetween :: UGen -> UGen -> UGen
indexInBetween bufnum in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "IndexInBetween" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Index into a table with a signal, linear interpolated
--
--  IndexL [ControlRate,AudioRate] bufnum=0 in=0;    FILTER: TRUE
indexL :: UGen -> UGen -> UGen
indexL bufnum in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "IndexL" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Base class for info ugens
--
--  InfoUGenBase [InitialisationRate]
infoUGenBase :: Rate -> UGen
infoUGenBase rate = mkUGen Nothing [InitialisationRate] (Left rate) "InfoUGenBase" [] Nothing 1 (Special 0) NoId

-- | A leaky integrator.
--
--  Integrator [ControlRate,AudioRate] in=0 coef=1;    FILTER: TRUE
integrator :: UGen -> UGen -> UGen
integrator in_ coef = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Integrator" [in_,coef] Nothing 1 (Special 0) NoId

-- | Control to audio rate converter.
--
--  K2A [AudioRate] in=0
k2a :: UGen -> UGen
k2a in_ = mkUGen Nothing [AudioRate] (Left AudioRate) "K2A" [in_] Nothing 1 (Special 0) NoId

-- | Respond to the state of a key
--
--  KeyState [ControlRate] keycode=0 minval=0 maxval=1 lag=0.2
keyState :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
keyState rate keycode minval maxval lag_ = mkUGen Nothing [ControlRate] (Left rate) "KeyState" [keycode,minval,maxval,lag_] Nothing 1 (Special 0) NoId

-- | Key tracker
--
--  KeyTrack [ControlRate] chain=0 keydecay=2 chromaleak=0.5
keyTrack :: Rate -> UGen -> UGen -> UGen -> UGen
keyTrack rate chain keydecay chromaleak = mkUGen Nothing [ControlRate] (Left rate) "KeyTrack" [chain,keydecay,chromaleak] Nothing 1 (Special 0) NoId

-- | Sine oscillator bank
--
--  Klang [AudioRate] freqscale=1 freqoffset=0 *specificationsArrayRef=0;    MCE=1, REORDERS INPUTS: [2,0,1]
klang :: Rate -> UGen -> UGen -> UGen -> UGen
klang rate freqscale freqoffset specificationsArrayRef = mkUGen Nothing [AudioRate] (Left rate) "Klang" [freqscale,freqoffset] (Just [specificationsArrayRef]) 1 (Special 0) NoId

-- | Bank of resonators
--
--  Klank [AudioRate] input=0 freqscale=1 freqoffset=0 decayscale=1 *specificationsArrayRef=0;    MCE=1, FILTER: TRUE, REORDERS INPUTS: [4,0,1,2,3]
klank :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
klank input freqscale freqoffset decayscale specificationsArrayRef = mkUGen Nothing [AudioRate] (Right [0]) "Klank" [input,freqscale,freqoffset,decayscale] (Just [specificationsArrayRef]) 1 (Special 0) NoId

-- | Clipped noise
--
--  LFClipNoise [ControlRate,AudioRate] freq=500;    NONDET
lfClipNoiseId :: ID a => a -> Rate -> UGen -> UGen
lfClipNoiseId z rate freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFClipNoise" [freq] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFClipNoise.
lfClipNoiseM :: UId m => Rate -> UGen -> m UGen
lfClipNoiseM = liftUId2 lfClipNoiseId

-- | Unsafe variant of LFClipNoise.
lfClipNoise ::  Rate -> UGen -> UGen
lfClipNoise = liftUnsafe2 lfClipNoiseM

-- | A sine like shape made of two cubic pieces
--
--  LFCub [ControlRate,AudioRate] freq=440 iphase=0
lfCub :: Rate -> UGen -> UGen -> UGen
lfCub rate freq iphase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFCub" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Dynamic clipped noise
--
--  LFDClipNoise [ControlRate,AudioRate] freq=500;    NONDET
lfdClipNoiseId :: ID a => a -> Rate -> UGen -> UGen
lfdClipNoiseId z rate freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFDClipNoise" [freq] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFDClipNoise.
lfdClipNoiseM :: UId m => Rate -> UGen -> m UGen
lfdClipNoiseM = liftUId2 lfdClipNoiseId

-- | Unsafe variant of LFDClipNoise.
lfdClipNoise ::  Rate -> UGen -> UGen
lfdClipNoise = liftUnsafe2 lfdClipNoiseM

-- | Dynamic step noise
--
--  LFDNoise0 [ControlRate,AudioRate] freq=500;    NONDET
lfdNoise0Id :: ID a => a -> Rate -> UGen -> UGen
lfdNoise0Id z rate freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFDNoise0" [freq] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFDNoise0.
lfdNoise0M :: UId m => Rate -> UGen -> m UGen
lfdNoise0M = liftUId2 lfdNoise0Id

-- | Unsafe variant of LFDNoise0.
lfdNoise0 ::  Rate -> UGen -> UGen
lfdNoise0 = liftUnsafe2 lfdNoise0M

-- | Dynamic ramp noise
--
--  LFDNoise1 [ControlRate,AudioRate] freq=500;    NONDET
lfdNoise1Id :: ID a => a -> Rate -> UGen -> UGen
lfdNoise1Id z rate freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFDNoise1" [freq] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFDNoise1.
lfdNoise1M :: UId m => Rate -> UGen -> m UGen
lfdNoise1M = liftUId2 lfdNoise1Id

-- | Unsafe variant of LFDNoise1.
lfdNoise1 ::  Rate -> UGen -> UGen
lfdNoise1 = liftUnsafe2 lfdNoise1M

-- | Dynamic cubic noise
--
--  LFDNoise3 [ControlRate,AudioRate] freq=500;    NONDET
lfdNoise3Id :: ID a => a -> Rate -> UGen -> UGen
lfdNoise3Id z rate freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFDNoise3" [freq] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFDNoise3.
lfdNoise3M :: UId m => Rate -> UGen -> m UGen
lfdNoise3M = liftUId2 lfdNoise3Id

-- | Unsafe variant of LFDNoise3.
lfdNoise3 ::  Rate -> UGen -> UGen
lfdNoise3 = liftUnsafe2 lfdNoise3M

-- | Gaussian function oscillator
--
--  LFGauss [ControlRate,AudioRate] duration=1 width=0.1 iphase=0 loop=1 doneAction=0;    ENUMERATION INPUTS: 3=Loop, 4=DoneAction
lfGauss :: Rate -> UGen -> UGen -> UGen -> Loop UGen -> DoneAction UGen -> UGen
lfGauss rate duration width iphase loop doneAction = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFGauss" [duration,width,iphase,(from_loop loop),(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Step noise
--
--  LFNoise0 [ControlRate,AudioRate] freq=500;    NONDET
lfNoise0Id :: ID a => a -> Rate -> UGen -> UGen
lfNoise0Id z rate freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFNoise0" [freq] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFNoise0.
lfNoise0M :: UId m => Rate -> UGen -> m UGen
lfNoise0M = liftUId2 lfNoise0Id

-- | Unsafe variant of LFNoise0.
lfNoise0 ::  Rate -> UGen -> UGen
lfNoise0 = liftUnsafe2 lfNoise0M

-- | Ramp noise
--
--  LFNoise1 [ControlRate,AudioRate] freq=500;    NONDET
lfNoise1Id :: ID a => a -> Rate -> UGen -> UGen
lfNoise1Id z rate freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFNoise1" [freq] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFNoise1.
lfNoise1M :: UId m => Rate -> UGen -> m UGen
lfNoise1M = liftUId2 lfNoise1Id

-- | Unsafe variant of LFNoise1.
lfNoise1 ::  Rate -> UGen -> UGen
lfNoise1 = liftUnsafe2 lfNoise1M

-- | Quadratic noise.
--
--  LFNoise2 [ControlRate,AudioRate] freq=500;    NONDET
lfNoise2Id :: ID a => a -> Rate -> UGen -> UGen
lfNoise2Id z rate freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFNoise2" [freq] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFNoise2.
lfNoise2M :: UId m => Rate -> UGen -> m UGen
lfNoise2M = liftUId2 lfNoise2Id

-- | Unsafe variant of LFNoise2.
lfNoise2 ::  Rate -> UGen -> UGen
lfNoise2 = liftUnsafe2 lfNoise2M

-- | Parabolic oscillator
--
--  LFPar [ControlRate,AudioRate] freq=440 iphase=0
lfPar :: Rate -> UGen -> UGen -> UGen
lfPar rate freq iphase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFPar" [freq,iphase] Nothing 1 (Special 0) NoId

-- | pulse oscillator
--
--  LFPulse [ControlRate,AudioRate] freq=440 iphase=0 width=0.5
lfPulse :: Rate -> UGen -> UGen -> UGen -> UGen
lfPulse rate freq iphase width = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFPulse" [freq,iphase,width] Nothing 1 (Special 0) NoId

-- | Sawtooth oscillator
--
--  LFSaw [ControlRate,AudioRate] freq=440 iphase=0
lfSaw :: Rate -> UGen -> UGen -> UGen
lfSaw rate freq iphase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFSaw" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Triangle oscillator
--
--  LFTri [ControlRate,AudioRate] freq=440 iphase=0
lfTri :: Rate -> UGen -> UGen -> UGen
lfTri rate freq iphase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFTri" [freq,iphase] Nothing 1 (Special 0) NoId

-- | 2nd order Butterworth lowpass filter
--
--  LPF [ControlRate,AudioRate] in=0 freq=440;    FILTER: TRUE
lpf :: UGen -> UGen -> UGen
lpf in_ freq = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "LPF" [in_,freq] Nothing 1 (Special 0) NoId

-- | Two point average filter
--
--  LPZ1 [ControlRate,AudioRate] in=0;    FILTER: TRUE
lpz1 :: UGen -> UGen
lpz1 in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "LPZ1" [in_] Nothing 1 (Special 0) NoId

-- | Two zero fixed lowpass
--
--  LPZ2 [ControlRate,AudioRate] in=0;    FILTER: TRUE
lpz2 :: UGen -> UGen
lpz2 in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "LPZ2" [in_] Nothing 1 (Special 0) NoId

-- | Exponential lag
--
--  Lag [ControlRate,AudioRate] in=0 lagTime=0.1;    FILTER: TRUE
lag :: UGen -> UGen -> UGen
lag in_ lagTime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Lag" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Exponential lag
--
--  Lag2 [ControlRate,AudioRate] in=0 lagTime=0.1;    FILTER: TRUE
lag2 :: UGen -> UGen -> UGen
lag2 in_ lagTime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Lag2" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Exponential lag
--
--  Lag2UD [ControlRate,AudioRate] in=0 lagTimeU=0.1 lagTimeD=0.1;    FILTER: TRUE
lag2UD :: UGen -> UGen -> UGen -> UGen
lag2UD in_ lagTimeU lagTimeD = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Lag2UD" [in_,lagTimeU,lagTimeD] Nothing 1 (Special 0) NoId

-- | Exponential lag
--
--  Lag3 [ControlRate,AudioRate] in=0 lagTime=0.1;    FILTER: TRUE
lag3 :: UGen -> UGen -> UGen
lag3 in_ lagTime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Lag3" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Exponential lag
--
--  Lag3UD [ControlRate,AudioRate] in=0 lagTimeU=0.1 lagTimeD=0.1;    FILTER: TRUE
lag3UD :: UGen -> UGen -> UGen -> UGen
lag3UD in_ lagTimeU lagTimeD = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Lag3UD" [in_,lagTimeU,lagTimeD] Nothing 1 (Special 0) NoId

-- | Read a control signal from a bus with a lag
--
--  LagIn [ControlRate] bus=0 lag=0.1;    NC INPUT: True
lagIn :: Int -> UGen -> UGen -> UGen
lagIn numChannels bus lag_ = mkUGen Nothing [ControlRate] (Left ControlRate) "LagIn" [bus,lag_] Nothing numChannels (Special 0) NoId

-- | Exponential lag
--
--  LagUD [ControlRate,AudioRate] in=0 lagTimeU=0.1 lagTimeD=0.1;    FILTER: TRUE
lagUD :: UGen -> UGen -> UGen -> UGen
lagUD in_ lagTimeU lagTimeD = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "LagUD" [in_,lagTimeU,lagTimeD] Nothing 1 (Special 0) NoId

-- | Output the last value before the input changed
--
--  LastValue [ControlRate,AudioRate] in=0 diff=0.01;    FILTER: TRUE
lastValue :: UGen -> UGen -> UGen
lastValue in_ diff = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "LastValue" [in_,diff] Nothing 1 (Special 0) NoId

-- | Sample and hold
--
--  Latch [ControlRate,AudioRate] in=0 trig=0;    FILTER: TRUE
latch :: UGen -> UGen -> UGen
latch in_ trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0,1]) "Latch" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Latoocarfian chaotic generator
--
--  LatoocarfianC [AudioRate] freq=22050 a=1 b=3 c=0.5 d=0.5 xi=0.5 yi=0.5
latoocarfianC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianC rate freq a b c d xi yi = mkUGen Nothing [AudioRate] (Left rate) "LatoocarfianC" [freq,a,b,c,d,xi,yi] Nothing 1 (Special 0) NoId

-- | Latoocarfian chaotic generator
--
--  LatoocarfianL [AudioRate] freq=22050 a=1 b=3 c=0.5 d=0.5 xi=0.5 yi=0.5
latoocarfianL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianL rate freq a b c d xi yi = mkUGen Nothing [AudioRate] (Left rate) "LatoocarfianL" [freq,a,b,c,d,xi,yi] Nothing 1 (Special 0) NoId

-- | Latoocarfian chaotic generator
--
--  LatoocarfianN [AudioRate] freq=22050 a=1 b=3 c=0.5 d=0.5 xi=0.5 yi=0.5
latoocarfianN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianN rate freq a b c d xi yi = mkUGen Nothing [AudioRate] (Left rate) "LatoocarfianN" [freq,a,b,c,d,xi,yi] Nothing 1 (Special 0) NoId

-- | Remove DC
--
--  LeakDC [ControlRate,AudioRate] in=0 coef=0.995;    FILTER: TRUE
leakDC :: UGen -> UGen -> UGen
leakDC in_ coef = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "LeakDC" [in_,coef] Nothing 1 (Special 0) NoId

-- | Output least changed
--
--  LeastChange [ControlRate,AudioRate] a=0 b=0
leastChange :: Rate -> UGen -> UGen -> UGen
leastChange rate a b = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LeastChange" [a,b] Nothing 1 (Special 0) NoId

-- | Peak limiter
--
--  Limiter [AudioRate] in=0 level=1 dur=0.01;    FILTER: TRUE
limiter :: UGen -> UGen -> UGen -> UGen
limiter in_ level dur = mkUGen Nothing [AudioRate] (Right [0]) "Limiter" [in_,level,dur] Nothing 1 (Special 0) NoId

-- | Linear congruential chaotic generator
--
--  LinCongC [AudioRate] freq=22050 a=1.1 c=0.13 m=1 xi=0
linCongC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongC rate freq a c m xi = mkUGen Nothing [AudioRate] (Left rate) "LinCongC" [freq,a,c,m,xi] Nothing 1 (Special 0) NoId

-- | Linear congruential chaotic generator
--
--  LinCongL [AudioRate] freq=22050 a=1.1 c=0.13 m=1 xi=0
linCongL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongL rate freq a c m xi = mkUGen Nothing [AudioRate] (Left rate) "LinCongL" [freq,a,c,m,xi] Nothing 1 (Special 0) NoId

-- | Linear congruential chaotic generator
--
--  LinCongN [AudioRate] freq=22050 a=1.1 c=0.13 m=1 xi=0
linCongN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongN rate freq a c m xi = mkUGen Nothing [AudioRate] (Left rate) "LinCongN" [freq,a,c,m,xi] Nothing 1 (Special 0) NoId

-- | Map a linear range to an exponential range
--
--  LinExp [InitialisationRate,ControlRate,AudioRate] in=0 srclo=0 srchi=1 dstlo=1 dsthi=2;    FILTER: TRUE
linExp :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linExp in_ srclo srchi dstlo dsthi = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Right [0]) "LinExp" [in_,srclo,srchi,dstlo,dsthi] Nothing 1 (Special 0) NoId

-- | Two channel linear pan.
--
--  LinPan2 [ControlRate,AudioRate] in=0 pos=0 level=1;    FILTER: TRUE
linPan2 :: UGen -> UGen -> UGen -> UGen
linPan2 in_ pos level = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "LinPan2" [in_,pos,level] Nothing 2 (Special 0) NoId

-- | Skewed random number generator.
--
--  LinRand [InitialisationRate] lo=0 hi=1 minmax=0;    NONDET
linRandId :: ID a => a -> UGen -> UGen -> UGen -> UGen
linRandId z lo hi minmax = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "LinRand" [lo,hi,minmax] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LinRand.
linRandM :: UId m => UGen -> UGen -> UGen -> m UGen
linRandM = liftUId3 linRandId

-- | Unsafe variant of LinRand.
linRand ::  UGen -> UGen -> UGen -> UGen
linRand = liftUnsafe3 linRandM

-- | Two channel linear crossfade.
--
--  LinXFade2 [ControlRate,AudioRate] inA=0 inB=0 pan=0 level=1;    FILTER: TRUE, PSUEDO INPUTS: [3]
linXFade2 :: UGen -> UGen -> UGen -> UGen
linXFade2 inA inB pan = mkUGen Nothing [ControlRate,AudioRate] (Right [0,1]) "LinXFade2" [inA,inB,pan] Nothing 1 (Special 0) NoId

-- | Line generator.
--
--  Line [ControlRate,AudioRate] start=0 end=1 dur=1 doneAction=0;    ENUMERATION INPUTS: 3=DoneAction
line :: Rate -> UGen -> UGen -> UGen -> DoneAction UGen -> UGen
line rate start end dur doneAction = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Line" [start,end,dur,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Simple linear envelope generator.
--
--  Linen [ControlRate] gate=1 attackTime=0.01 susLevel=1 releaseTime=1 doneAction=0;    ENUMERATION INPUTS: 4=DoneAction
linen :: UGen -> UGen -> UGen -> UGen -> DoneAction UGen -> UGen
linen gate_ attackTime susLevel releaseTime doneAction = mkUGen Nothing [ControlRate] (Left ControlRate) "Linen" [gate_,attackTime,susLevel,releaseTime,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Allocate a buffer local to the synth
--
--  LocalBuf [InitialisationRate] numChannels=1 numFrames=1;    REORDERS INPUTS: [1,0], NONDET
localBufId :: ID a => a -> UGen -> UGen -> UGen
localBufId z numChannels numFrames = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "LocalBuf" [numChannels,numFrames] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LocalBuf.
localBufM :: UId m => UGen -> UGen -> m UGen
localBufM = liftUId2 localBufId

-- | Unsafe variant of LocalBuf.
localBuf ::  UGen -> UGen -> UGen
localBuf = liftUnsafe2 localBufM

-- | Define and read from buses local to a synth.
--
--  LocalIn [ControlRate,AudioRate] *default=0;    MCE=1, NC INPUT: True
localIn :: Int -> Rate -> UGen -> UGen
localIn numChannels rate default_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LocalIn" [] (Just [default_]) numChannels (Special 0) NoId

-- | Write to buses local to a synth.
--
--  LocalOut [ControlRate,AudioRate] *channelsArray=0;    MCE=1, FILTER: TRUE
localOut :: UGen -> UGen
localOut input = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "LocalOut" [] (Just [input]) 0 (Special 0) NoId

-- | Chaotic noise function
--
--  Logistic [ControlRate,AudioRate] chaosParam=3 freq=1000 init=0.5
logistic :: Rate -> UGen -> UGen -> UGen -> UGen
logistic rate chaosParam freq init_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Logistic" [chaosParam,freq,init_] Nothing 1 (Special 0) NoId

-- | Lorenz chaotic generator
--
--  LorenzL [AudioRate] freq=22050 s=10 r=28 b=2.667 h=0.05 xi=0.1 yi=0 zi=0
lorenzL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenzL rate freq s r b h xi yi zi = mkUGen Nothing [AudioRate] (Left rate) "LorenzL" [freq,s,r,b,h,xi,yi,zi] Nothing 1 (Special 0) NoId

-- | Extraction of instantaneous loudness in sones
--
--  Loudness [ControlRate] chain=0 smask=0.25 tmask=1
loudness :: UGen -> UGen -> UGen -> UGen
loudness chain smask tmask = mkUGen Nothing [ControlRate] (Left ControlRate) "Loudness" [chain,smask,tmask] Nothing 1 (Special 0) NoId

-- | Mel frequency cepstral coefficients
--
--  MFCC [ControlRate] chain=0 numcoeff=13
mfcc :: Rate -> UGen -> UGen -> UGen
mfcc rate chain numcoeff = mkUGen Nothing [ControlRate] (Left rate) "MFCC" [chain,numcoeff] Nothing 13 (Special 0) NoId

-- | Reduce precision.
--
--  MantissaMask [ControlRate,AudioRate] in=0 bits=3;    FILTER: TRUE
mantissaMask :: UGen -> UGen -> UGen
mantissaMask in_ bits = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "MantissaMask" [in_,bits] Nothing 1 (Special 0) NoId

-- | Median filter.
--
--  Median [ControlRate,AudioRate] length=3 in=0;    FILTER: TRUE
median :: UGen -> UGen -> UGen
median length_ in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "Median" [length_,in_] Nothing 1 (Special 0) NoId

-- | Parametric filter.
--
--  MidEQ [ControlRate,AudioRate] in=0 freq=440 rq=1 db=0;    FILTER: TRUE
midEQ :: UGen -> UGen -> UGen -> UGen -> UGen
midEQ in_ freq rq db = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "MidEQ" [in_,freq,rq,db] Nothing 1 (Special 0) NoId

-- | Minimum difference of two values in modulo arithmetics
--
--  ModDif [InitialisationRate,ControlRate,AudioRate] x=0 y=0 mod=1;    FILTER: TRUE
modDif :: UGen -> UGen -> UGen -> UGen
modDif x y mod_ = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Right [0]) "ModDif" [x,y,mod_] Nothing 1 (Special 0) NoId

-- | Moog VCF implementation, designed by Federico Fontana
--
--  MoogFF [ControlRate,AudioRate] in=0 freq=100 gain=2 reset=0;    FILTER: TRUE
moogFF :: UGen -> UGen -> UGen -> UGen -> UGen
moogFF in_ freq gain reset = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "MoogFF" [in_,freq,gain,reset] Nothing 1 (Special 0) NoId

-- | Output most changed.
--
--  MostChange [ControlRate,AudioRate] a=0 b=0;    FILTER: TRUE
mostChange :: UGen -> UGen -> UGen
mostChange a b = mkUGen Nothing [ControlRate,AudioRate] (Right [0,1]) "MostChange" [a,b] Nothing 1 (Special 0) NoId

-- | Mouse button UGen.
--
--  MouseButton [ControlRate] minval=0 maxval=1 lag=0.2
mouseButton :: Rate -> UGen -> UGen -> UGen -> UGen
mouseButton rate minval maxval lag_ = mkUGen Nothing [ControlRate] (Left rate) "MouseButton" [minval,maxval,lag_] Nothing 1 (Special 0) NoId

-- | Cursor tracking UGen.
--
--  MouseX [ControlRate] minval=0 maxval=1 warp=0 lag=0.2;    ENUMERATION INPUTS: 2=Warp
mouseX :: Rate -> UGen -> UGen -> Warp UGen -> UGen -> UGen
mouseX rate minval maxval warp lag_ = mkUGen Nothing [ControlRate] (Left rate) "MouseX" [minval,maxval,(from_warp warp),lag_] Nothing 1 (Special 0) NoId

-- | Cursor tracking UGen.
--
--  MouseY [ControlRate] minval=0 maxval=1 warp=0 lag=0.2;    ENUMERATION INPUTS: 2=Warp
mouseY :: Rate -> UGen -> UGen -> Warp UGen -> UGen -> UGen
mouseY rate minval maxval warp lag_ = mkUGen Nothing [ControlRate] (Left rate) "MouseY" [minval,maxval,(from_warp warp),lag_] Nothing 1 (Special 0) NoId

-- | Sum of uniform distributions.
--
--  NRand [InitialisationRate] lo=0 hi=1 n=0;    NONDET
nRandId :: ID a => a -> UGen -> UGen -> UGen -> UGen
nRandId z lo hi n = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "NRand" [lo,hi,n] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of NRand.
nRandM :: UId m => UGen -> UGen -> UGen -> m UGen
nRandM = liftUId3 nRandId

-- | Unsafe variant of NRand.
nRand ::  UGen -> UGen -> UGen -> UGen
nRand = liftUnsafe3 nRandM

-- | (Undocumented class)
--
--  NodeID [InitialisationRate]
nodeID :: Rate -> UGen
nodeID rate = mkUGen Nothing [InitialisationRate] (Left rate) "NodeID" [] Nothing 1 (Special 0) NoId

-- | Flattens dynamics.
--
--  Normalizer [AudioRate] in=0 level=1 dur=0.01;    FILTER: TRUE
normalizer :: UGen -> UGen -> UGen -> UGen
normalizer in_ level dur = mkUGen Nothing [AudioRate] (Right [0]) "Normalizer" [in_,level,dur] Nothing 1 (Special 0) NoId

-- | Number of audio busses.
--
--  NumAudioBuses [InitialisationRate]
numAudioBuses :: UGen
numAudioBuses = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "NumAudioBuses" [] Nothing 1 (Special 0) NoId

-- | Number of open buffers.
--
--  NumBuffers [InitialisationRate]
numBuffers :: UGen
numBuffers = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "NumBuffers" [] Nothing 1 (Special 0) NoId

-- | Number of control busses.
--
--  NumControlBuses [InitialisationRate]
numControlBuses :: UGen
numControlBuses = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "NumControlBuses" [] Nothing 1 (Special 0) NoId

-- | Number of input busses.
--
--  NumInputBuses [InitialisationRate]
numInputBuses :: UGen
numInputBuses = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "NumInputBuses" [] Nothing 1 (Special 0) NoId

-- | Number of output busses.
--
--  NumOutputBuses [InitialisationRate]
numOutputBuses :: UGen
numOutputBuses = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "NumOutputBuses" [] Nothing 1 (Special 0) NoId

-- | Number of currently running synths.
--
--  NumRunningSynths [InitialisationRate,ControlRate]
numRunningSynths :: UGen
numRunningSynths = mkUGen Nothing [InitialisationRate,ControlRate] (Left InitialisationRate) "NumRunningSynths" [] Nothing 1 (Special 0) NoId

-- | Write a signal to a bus with sample accurate timing.
--
--  OffsetOut [ControlRate,AudioRate] bus=0 *channelsArray=0;    MCE=1, FILTER: TRUE
offsetOut :: UGen -> UGen -> UGen
offsetOut bus input = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "OffsetOut" [bus] (Just [input]) 0 (Special 0) NoId

-- | One pole filter.
--
--  OnePole [ControlRate,AudioRate] in=0 coef=0.5;    FILTER: TRUE
onePole :: UGen -> UGen -> UGen
onePole in_ coef = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "OnePole" [in_,coef] Nothing 1 (Special 0) NoId

-- | One zero filter.
--
--  OneZero [ControlRate,AudioRate] in=0 coef=0.5;    FILTER: TRUE
oneZero :: UGen -> UGen -> UGen
oneZero in_ coef = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "OneZero" [in_,coef] Nothing 1 (Special 0) NoId

-- | Onset detector
--
--  Onsets [ControlRate] chain=0 threshold=0.5 odftype=3 relaxtime=1 floor=0.1 mingap=10 medianspan=11 whtype=1 rawodf=0
onsets :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
onsets chain threshold odftype relaxtime floor_ mingap medianspan whtype rawodf = mkUGen Nothing [ControlRate] (Left ControlRate) "Onsets" [chain,threshold,odftype,relaxtime,floor_,mingap,medianspan,whtype,rawodf] Nothing 1 (Special 0) NoId

-- | Interpolating wavetable oscillator.
--
--  Osc [ControlRate,AudioRate] bufnum=0 freq=440 phase=0
osc :: Rate -> UGen -> UGen -> UGen -> UGen
osc rate bufnum freq phase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Osc" [bufnum,freq,phase] Nothing 1 (Special 0) NoId

-- | Noninterpolating wavetable oscillator.
--
--  OscN [ControlRate,AudioRate] bufnum=0 freq=440 phase=0
oscN :: Rate -> UGen -> UGen -> UGen -> UGen
oscN rate bufnum freq phase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "OscN" [bufnum,freq,phase] Nothing 1 (Special 0) NoId

-- | Write a signal to a bus.
--
--  Out [ControlRate,AudioRate] bus=0 *channelsArray=0;    MCE=1, FILTER: TRUE
out :: UGen -> UGen -> UGen
out bus input = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "Out" [bus] (Just [input]) 0 (Special 0) NoId

-- | Very fast sine grain with a parabolic envelope
--
--  PSinGrain [AudioRate] freq=440 dur=0.2 amp=0.1
pSinGrain :: Rate -> UGen -> UGen -> UGen -> UGen
pSinGrain rate freq dur amp = mkUGen Nothing [AudioRate] (Left rate) "PSinGrain" [freq,dur,amp] Nothing 1 (Special 0) NoId

-- | Complex addition.
--
--  PV_Add [ControlRate] bufferA=0 bufferB=0
pv_Add :: UGen -> UGen -> UGen
pv_Add bufferA bufferB = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Add" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Scramble bins.
--
--  PV_BinScramble [ControlRate] buffer=0 wipe=0 width=0.2 trig=0;    NONDET
pv_BinScrambleId :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScrambleId z buffer wipe width trig_ = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_BinScramble" [buffer,wipe,width,trig_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of PV_BinScramble.
pv_BinScrambleM :: UId m => UGen -> UGen -> UGen -> UGen -> m UGen
pv_BinScrambleM = liftUId4 pv_BinScrambleId

-- | Unsafe variant of PV_BinScramble.
pv_BinScramble ::  UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble = liftUnsafe4 pv_BinScrambleM

-- | Shift and stretch bin position.
--
--  PV_BinShift [ControlRate] buffer=0 stretch=1 shift=0 interp=0
pv_BinShift :: UGen -> UGen -> UGen -> UGen -> UGen
pv_BinShift buffer stretch shift interp = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_BinShift" [buffer,stretch,shift,interp] Nothing 1 (Special 0) NoId

-- | Combine low and high bins from two inputs.
--
--  PV_BinWipe [ControlRate] bufferA=0 bufferB=0 wipe=0
pv_BinWipe :: UGen -> UGen -> UGen -> UGen
pv_BinWipe bufferA bufferB wipe = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_BinWipe" [bufferA,bufferB,wipe] Nothing 1 (Special 0) NoId

-- | Zero bins.
--
--  PV_BrickWall [ControlRate] buffer=0 wipe=0
pv_BrickWall :: UGen -> UGen -> UGen
pv_BrickWall buffer wipe = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_BrickWall" [buffer,wipe] Nothing 1 (Special 0) NoId

-- | Complex plane attack.
--
--  PV_ConformalMap [ControlRate] buffer=0 areal=0 aimag=0
pv_ConformalMap :: UGen -> UGen -> UGen -> UGen
pv_ConformalMap buffer areal aimag = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_ConformalMap" [buffer,areal,aimag] Nothing 1 (Special 0) NoId

-- | Complex conjugate
--
--  PV_Conj [ControlRate] buffer=0
pv_Conj :: UGen -> UGen
pv_Conj buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Conj" [buffer] Nothing 1 (Special 0) NoId

-- | Copy an FFT buffer
--
--  PV_Copy [ControlRate] bufferA=0 bufferB=0
pv_Copy :: UGen -> UGen -> UGen
pv_Copy bufferA bufferB = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Copy" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Copy magnitudes and phases.
--
--  PV_CopyPhase [ControlRate] bufferA=0 bufferB=0
pv_CopyPhase :: UGen -> UGen -> UGen
pv_CopyPhase bufferA bufferB = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_CopyPhase" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Random phase shifting.
--
--  PV_Diffuser [ControlRate] buffer=0 trig=0
pv_Diffuser :: UGen -> UGen -> UGen
pv_Diffuser buffer trig_ = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Diffuser" [buffer,trig_] Nothing 1 (Special 0) NoId

-- | Complex division
--
--  PV_Div [ControlRate] bufferA=0 bufferB=0
pv_Div :: UGen -> UGen -> UGen
pv_Div bufferA bufferB = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Div" [bufferA,bufferB] Nothing 1 (Special 0) NoId

{-
-- | FFT onset detector.
--
--  PV_HainsworthFoote [ControlRate,AudioRate] maxSize=0
pv_HainsworthFoote :: UGen -> UGen
pv_HainsworthFoote maxSize = mkUGen Nothing [ControlRate,AudioRate] (Left ControlRate) "PV_HainsworthFoote" [maxSize] Nothing 1 (Special 0) NoId

-- | FFT feature detector for onset detection.
--
--  PV_JensenAndersen [ControlRate,AudioRate] maxSize=0
pv_JensenAndersen :: UGen -> UGen
pv_JensenAndersen maxSize = mkUGen Nothing [ControlRate,AudioRate] (Left ControlRate) "PV_JensenAndersen" [maxSize] Nothing 1 (Special 0) NoId
-}

-- | Pass bins which are a local maximum.
--
--  PV_LocalMax [ControlRate] buffer=0 threshold=0
pv_LocalMax :: UGen -> UGen -> UGen
pv_LocalMax buffer threshold = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_LocalMax" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Pass bins above a threshold.
--
--  PV_MagAbove [ControlRate] buffer=0 threshold=0
pv_MagAbove :: UGen -> UGen -> UGen
pv_MagAbove buffer threshold = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagAbove" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Pass bins below a threshold.
--
--  PV_MagBelow [ControlRate] buffer=0 threshold=0
pv_MagBelow :: UGen -> UGen -> UGen
pv_MagBelow buffer threshold = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagBelow" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Clip bins to a threshold.
--
--  PV_MagClip [ControlRate] buffer=0 threshold=0
pv_MagClip :: UGen -> UGen -> UGen
pv_MagClip buffer threshold = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagClip" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Division of magnitudes
--
--  PV_MagDiv [ControlRate] bufferA=0 bufferB=0 zeroed=0.0001
pv_MagDiv :: UGen -> UGen -> UGen -> UGen
pv_MagDiv bufferA bufferB zeroed = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagDiv" [bufferA,bufferB,zeroed] Nothing 1 (Special 0) NoId

-- | Freeze magnitudes.
--
--  PV_MagFreeze [ControlRate] buffer=0 freeze=0
pv_MagFreeze :: UGen -> UGen -> UGen
pv_MagFreeze buffer freeze = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagFreeze" [buffer,freeze] Nothing 1 (Special 0) NoId

-- | Multiply magnitudes.
--
--  PV_MagMul [ControlRate] bufferA=0 bufferB=0
pv_MagMul :: UGen -> UGen -> UGen
pv_MagMul bufferA bufferB = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagMul" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Multiply magnitudes by noise.
--
--  PV_MagNoise [ControlRate] buffer=0
pv_MagNoise :: UGen -> UGen
pv_MagNoise buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagNoise" [buffer] Nothing 1 (Special 0) NoId

-- | shift and stretch magnitude bin position.
--
--  PV_MagShift [ControlRate] buffer=0 stretch=1 shift=0
pv_MagShift :: UGen -> UGen -> UGen -> UGen
pv_MagShift buffer stretch shift = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagShift" [buffer,stretch,shift] Nothing 1 (Special 0) NoId

-- | Average magnitudes across bins.
--
--  PV_MagSmear [ControlRate] buffer=0 bins=0
pv_MagSmear :: UGen -> UGen -> UGen
pv_MagSmear buffer bins = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagSmear" [buffer,bins] Nothing 1 (Special 0) NoId

-- | Square magnitudes.
--
--  PV_MagSquared [ControlRate] buffer=0
pv_MagSquared :: UGen -> UGen
pv_MagSquared buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagSquared" [buffer] Nothing 1 (Special 0) NoId

-- | Maximum magnitude.
--
--  PV_Max [ControlRate] bufferA=0 bufferB=0
pv_Max :: UGen -> UGen -> UGen
pv_Max bufferA bufferB = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Max" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Minimum magnitude.
--
--  PV_Min [ControlRate] bufferA=0 bufferB=0
pv_Min :: UGen -> UGen -> UGen
pv_Min bufferA bufferB = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Min" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Complex multiply.
--
--  PV_Mul [ControlRate] bufferA=0 bufferB=0
pv_Mul :: UGen -> UGen -> UGen
pv_Mul bufferA bufferB = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Mul" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Shift phase.
--
--  PV_PhaseShift [ControlRate] buffer=0 shift=0 integrate=0
pv_PhaseShift :: UGen -> UGen -> UGen -> UGen
pv_PhaseShift buffer shift integrate = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_PhaseShift" [buffer,shift,integrate] Nothing 1 (Special 0) NoId

-- | Shift phase by 270 degrees.
--
--  PV_PhaseShift270 [ControlRate] buffer=0
pv_PhaseShift270 :: UGen -> UGen
pv_PhaseShift270 buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_PhaseShift270" [buffer] Nothing 1 (Special 0) NoId

-- | Shift phase by 90 degrees.
--
--  PV_PhaseShift90 [ControlRate] buffer=0
pv_PhaseShift90 :: UGen -> UGen
pv_PhaseShift90 buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_PhaseShift90" [buffer] Nothing 1 (Special 0) NoId

-- | Pass random bins.
--
--  PV_RandComb [ControlRate] buffer=0 wipe=0 trig=0;    NONDET
pv_RandCombId :: ID a => a -> UGen -> UGen -> UGen -> UGen
pv_RandCombId z buffer wipe trig_ = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_RandComb" [buffer,wipe,trig_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of PV_RandComb.
pv_RandCombM :: UId m => UGen -> UGen -> UGen -> m UGen
pv_RandCombM = liftUId3 pv_RandCombId

-- | Unsafe variant of PV_RandComb.
pv_RandComb ::  UGen -> UGen -> UGen -> UGen
pv_RandComb = liftUnsafe3 pv_RandCombM

-- | Crossfade in random bin order.
--
--  PV_RandWipe [ControlRate] bufferA=0 bufferB=0 wipe=0 trig=0;    NONDET
pv_RandWipeId :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipeId z bufferA bufferB wipe trig_ = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_RandWipe" [bufferA,bufferB,wipe,trig_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of PV_RandWipe.
pv_RandWipeM :: UId m => UGen -> UGen -> UGen -> UGen -> m UGen
pv_RandWipeM = liftUId4 pv_RandWipeId

-- | Unsafe variant of PV_RandWipe.
pv_RandWipe ::  UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe = liftUnsafe4 pv_RandWipeM

-- | Make gaps in spectrum.
--
--  PV_RectComb [ControlRate] buffer=0 numTeeth=0 phase=0 width=0.5
pv_RectComb :: UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb buffer numTeeth phase width = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_RectComb" [buffer,numTeeth,phase,width] Nothing 1 (Special 0) NoId

-- | Make gaps in spectrum.
--
--  PV_RectComb2 [ControlRate] bufferA=0 bufferB=0 numTeeth=0 phase=0 width=0.5
pv_RectComb2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb2 bufferA bufferB numTeeth phase width = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_RectComb2" [bufferA,bufferB,numTeeth,phase,width] Nothing 1 (Special 0) NoId

-- | Two channel equal power pan.
--
--  Pan2 [ControlRate,AudioRate] in=0 pos=0 level=1;    FILTER: TRUE
pan2 :: UGen -> UGen -> UGen -> UGen
pan2 in_ pos level = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Pan2" [in_,pos,level] Nothing 2 (Special 0) NoId

-- | Four channel equal power pan.
--
--  Pan4 [ControlRate,AudioRate] in=0 xpos=0 ypos=0 level=1
pan4 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
pan4 rate in_ xpos ypos level = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Pan4" [in_,xpos,ypos,level] Nothing 4 (Special 0) NoId

-- | Azimuth panner
--
--  PanAz [ControlRate,AudioRate] in=0 pos=0 level=1 width=2 orientation=0.5;    NC INPUT: True, FILTER: TRUE
panAz :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
panAz numChannels in_ pos level width orientation = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "PanAz" [in_,pos,level,width,orientation] Nothing numChannels (Special 0) NoId

-- | Ambisonic B-format panner.
--
--  PanB [ControlRate,AudioRate] in=0 azimuth=0 elevation=0 gain=1
panB :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
panB rate in_ azimuth elevation gain = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "PanB" [in_,azimuth,elevation,gain] Nothing 4 (Special 0) NoId

-- | 2D Ambisonic B-format panner.
--
--  PanB2 [ControlRate,AudioRate] in=0 azimuth=0 gain=1;    FILTER: TRUE
panB2 :: UGen -> UGen -> UGen -> UGen
panB2 in_ azimuth gain = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "PanB2" [in_,azimuth,gain] Nothing 3 (Special 0) NoId

-- | Real-time partitioned convolution
--
--  PartConv [AudioRate] in=0 fftsize=0 irbufnum=0
partConv :: UGen -> UGen -> UGen -> UGen
partConv in_ fftsize irbufnum = mkUGen Nothing [AudioRate] (Left AudioRate) "PartConv" [in_,fftsize,irbufnum] Nothing 1 (Special 0) NoId

-- | When triggered, pauses a node.
--
--  Pause [ControlRate] gate=0 id=0
pause :: UGen -> UGen -> UGen
pause gate_ id_ = mkUGen Nothing [ControlRate] (Left ControlRate) "Pause" [gate_,id_] Nothing 1 (Special 0) NoId

-- | When triggered, pause enclosing synth.
--
--  PauseSelf [ControlRate] in=0
pauseSelf :: UGen -> UGen
pauseSelf in_ = mkUGen Nothing [ControlRate] (Left ControlRate) "PauseSelf" [in_] Nothing 1 (Special 0) NoId

-- | FIXME: PauseSelfWhenDone purpose.
--
--  PauseSelfWhenDone [ControlRate] src=0
pauseSelfWhenDone :: UGen -> UGen
pauseSelfWhenDone src = mkUGen Nothing [ControlRate] (Left ControlRate) "PauseSelfWhenDone" [src] Nothing 1 (Special 0) NoId

-- | Track peak signal amplitude.
--
--  Peak [ControlRate,AudioRate] in=0 trig=0;    FILTER: TRUE
peak :: UGen -> UGen -> UGen
peak in_ trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Peak" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Track peak signal amplitude.
--
--  PeakFollower [ControlRate,AudioRate] in=0 decay=0.999;    FILTER: TRUE
peakFollower :: UGen -> UGen -> UGen
peakFollower in_ decay_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "PeakFollower" [in_,decay_] Nothing 1 (Special 0) NoId

-- | A resettable linear ramp between two levels.
--
--  Phasor [ControlRate,AudioRate] trig=0 rate=1 start=0 end=1 resetPos=0
phasor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
phasor rate trig_ rate_ start end resetPos = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Phasor" [trig_,rate_,start,end,resetPos] Nothing 1 (Special 0) NoId

-- | Pink Noise.
--
--  PinkNoise [ControlRate,AudioRate] ;    NONDET
pinkNoiseId :: ID a => a -> Rate -> UGen
pinkNoiseId z rate = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "PinkNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of PinkNoise.
pinkNoiseM :: UId m => Rate -> m UGen
pinkNoiseM = liftUId1 pinkNoiseId

-- | Unsafe variant of PinkNoise.
pinkNoise ::  Rate -> UGen
pinkNoise = liftUnsafe1 pinkNoiseM

-- | Autocorrelation pitch follower
--
--  Pitch [ControlRate] in=0 initFreq=440 minFreq=60 maxFreq=4000 execFreq=100 maxBinsPerOctave=16 median=1 ampThreshold=0.01 peakThreshold=0.5 downSample=1 clar=0
pitch :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pitch in_ initFreq minFreq maxFreq execFreq maxBinsPerOctave median_ ampThreshold peakThreshold downSample clar = mkUGen Nothing [ControlRate] (Left ControlRate) "Pitch" [in_,initFreq,minFreq,maxFreq,execFreq,maxBinsPerOctave,median_,ampThreshold,peakThreshold,downSample,clar] Nothing 2 (Special 0) NoId

-- | Time domain pitch shifter.
--
--  PitchShift [AudioRate] in=0 windowSize=0.2 pitchRatio=1 pitchDispersion=0 timeDispersion=0;    FILTER: TRUE
pitchShift :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pitchShift in_ windowSize pitchRatio pitchDispersion timeDispersion = mkUGen Nothing [AudioRate] (Right [0]) "PitchShift" [in_,windowSize,pitchRatio,pitchDispersion,timeDispersion] Nothing 1 (Special 0) NoId

-- | Sample playback oscillator.
--
--  PlayBuf [ControlRate,AudioRate] bufnum=0 rate=1 trigger=1 startPos=0 loop=0 doneAction=0;    NC INPUT: True, ENUMERATION INPUTS: 4=Loop, 5=DoneAction
playBuf :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> Loop UGen -> DoneAction UGen -> UGen
playBuf numChannels rate bufnum rate_ trigger startPos loop doneAction = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "PlayBuf" [bufnum,rate_,trigger,startPos,(from_loop loop),(from_done_action doneAction)] Nothing numChannels (Special 0) NoId

-- | A Karplus-Strong UGen
--
--  Pluck [AudioRate] in=0 trig=1 maxdelaytime=0.2 delaytime=0.2 decaytime=1 coef=0.5;    FILTER: TRUE
pluck :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pluck in_ trig_ maxdelaytime delaytime decaytime coef = mkUGen Nothing [AudioRate] (Right [0]) "Pluck" [in_,trig_,maxdelaytime,delaytime,decaytime,coef] Nothing 1 (Special 0) NoId

{-
-- | Print the current output value of a UGen
--
--  Poll [ControlRate,AudioRate] trig=0 in=0 trigid=-1 label=0;    FILTER: TRUE, REORDERS INPUTS: [0,1,3,2]
poll :: UGen -> UGen -> UGen -> UGen -> UGen
poll trig_ in_ trigid label_ = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "Poll" [trig_,in_,trigid,label_] Nothing 1 (Special 0) NoId
-}

-- | Band limited pulse wave.
--
--  Pulse [ControlRate,AudioRate] freq=440 width=0.5
pulse :: Rate -> UGen -> UGen -> UGen
pulse rate freq width = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Pulse" [freq,width] Nothing 1 (Special 0) NoId

-- | Pulse counter.
--
--  PulseCount [ControlRate,AudioRate] trig=0 reset=0;    FILTER: TRUE
pulseCount :: UGen -> UGen -> UGen
pulseCount trig_ reset = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "PulseCount" [trig_,reset] Nothing 1 (Special 0) NoId

-- | Pulse divider.
--
--  PulseDivider [ControlRate,AudioRate] trig=0 div=2 start=0;    FILTER: TRUE
pulseDivider :: UGen -> UGen -> UGen -> UGen
pulseDivider trig_ div_ start = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "PulseDivider" [trig_,div_,start] Nothing 1 (Special 0) NoId

-- | General quadratic map chaotic generator
--
--  QuadC [AudioRate] freq=22050 a=1 b=-1 c=-0.75 xi=0
quadC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadC rate freq a b c xi = mkUGen Nothing [AudioRate] (Left rate) "QuadC" [freq,a,b,c,xi] Nothing 1 (Special 0) NoId

-- | General quadratic map chaotic generator
--
--  QuadL [AudioRate] freq=22050 a=1 b=-1 c=-0.75 xi=0
quadL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadL rate freq a b c xi = mkUGen Nothing [AudioRate] (Left rate) "QuadL" [freq,a,b,c,xi] Nothing 1 (Special 0) NoId

-- | General quadratic map chaotic generator
--
--  QuadN [AudioRate] freq=22050 a=1 b=-1 c=-0.75 xi=0
quadN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadN rate freq a b c xi = mkUGen Nothing [AudioRate] (Left rate) "QuadN" [freq,a,b,c,xi] Nothing 1 (Special 0) NoId

-- | A resonant high pass filter.
--
--  RHPF [ControlRate,AudioRate] in=0 freq=440 rq=1;    FILTER: TRUE
rhpf :: UGen -> UGen -> UGen -> UGen
rhpf in_ freq rq = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "RHPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | A resonant low pass filter.
--
--  RLPF [ControlRate,AudioRate] in=0 freq=440 rq=1;    FILTER: TRUE
rlpf :: UGen -> UGen -> UGen -> UGen
rlpf in_ freq rq = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "RLPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Number of radians per sample.
--
--  RadiansPerSample [InitialisationRate]
radiansPerSample :: UGen
radiansPerSample = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "RadiansPerSample" [] Nothing 1 (Special 0) NoId

-- | Break a continuous signal into line segments
--
--  Ramp [ControlRate,AudioRate] in=0 lagTime=0.1;    FILTER: TRUE
ramp :: UGen -> UGen -> UGen
ramp in_ lagTime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Ramp" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Single random number generator.
--
--  Rand [InitialisationRate] lo=0 hi=1;    NONDET
randId :: ID a => a -> UGen -> UGen -> UGen
randId z lo hi = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "Rand" [lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Rand.
randM :: UId m => UGen -> UGen -> m UGen
randM = liftUId2 randId

-- | Unsafe variant of Rand.
rand ::  UGen -> UGen -> UGen
rand = liftUnsafe2 randM

-- | Set the synth's random generator ID.
--
--  RandID [InitialisationRate,ControlRate] id=0
randID :: Rate -> UGen -> UGen
randID rate id_ = mkUGen Nothing [InitialisationRate,ControlRate] (Left rate) "RandID" [id_] Nothing 0 (Special 0) NoId

-- | Sets the synth's random generator seed.
--
--  RandSeed [InitialisationRate,ControlRate,AudioRate] trig=0 seed=56789
randSeed :: Rate -> UGen -> UGen -> UGen
randSeed rate trig_ seed = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Left rate) "RandSeed" [trig_,seed] Nothing 0 (Special 0) NoId

-- | Record or overdub into a Buffer.
--
--  RecordBuf [ControlRate,AudioRate] bufnum=0 offset=0 recLevel=1 preLevel=0 run=1 loop=1 trigger=1 doneAction=0 *inputArray=0;    MCE=1, REORDERS INPUTS: [8,0,1,2,3,4,5,6,7], ENUMERATION INPUTS: 5=Loop, 7=DoneAction
recordBuf :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> Loop UGen -> UGen -> DoneAction UGen -> UGen -> UGen
recordBuf rate bufnum offset recLevel preLevel run loop trigger doneAction inputArray = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "RecordBuf" [bufnum,offset,recLevel,preLevel,run,(from_loop loop),trigger,(from_done_action doneAction)] (Just [inputArray]) 1 (Special 0) NoId

-- | Send signal to a bus, overwriting previous contents.
--
--  ReplaceOut [ControlRate,AudioRate] bus=0 *channelsArray=0;    MCE=1, FILTER: TRUE
replaceOut :: UGen -> UGen -> UGen
replaceOut bus input = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "ReplaceOut" [bus] (Just [input]) 0 (Special 0) NoId

-- | Resonant filter.
--
--  Resonz [ControlRate,AudioRate] in=0 freq=440 bwr=1;    FILTER: TRUE
resonz :: UGen -> UGen -> UGen -> UGen
resonz in_ freq bwr = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Resonz" [in_,freq,bwr] Nothing 1 (Special 0) NoId

-- | Ringing filter.
--
--  Ringz [ControlRate,AudioRate] in=0 freq=440 decaytime=1;    FILTER: TRUE
ringz :: UGen -> UGen -> UGen -> UGen
ringz in_ freq decaytime = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Ringz" [in_,freq,decaytime] Nothing 1 (Special 0) NoId

-- | Rotate a sound field.
--
--  Rotate2 [ControlRate,AudioRate] x=0 y=0 pos=0;    FILTER: TRUE
rotate2 :: UGen -> UGen -> UGen -> UGen
rotate2 x y pos = mkUGen Nothing [ControlRate,AudioRate] (Right [0,1]) "Rotate2" [x,y,pos] Nothing 2 (Special 0) NoId

-- | Track maximum level.
--
--  RunningMax [ControlRate,AudioRate] in=0 trig=0;    FILTER: TRUE
runningMax :: UGen -> UGen -> UGen
runningMax in_ trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "RunningMax" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Track minimum level.
--
--  RunningMin [ControlRate,AudioRate] in=0 trig=0;    FILTER: TRUE
runningMin :: UGen -> UGen -> UGen
runningMin in_ trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "RunningMin" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Running sum over n frames
--
--  RunningSum [ControlRate,AudioRate] in=0 numsamp=40;    FILTER: TRUE
runningSum :: UGen -> UGen -> UGen
runningSum in_ numsamp = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "RunningSum" [in_,numsamp] Nothing 1 (Special 0) NoId

-- | Second order filter section (biquad).
--
--  SOS [ControlRate,AudioRate] in=0 a0=0 a1=0 a2=0 b1=0 b2=0;    FILTER: TRUE
sos :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sos in_ a0 a1 a2 b1 b2 = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "SOS" [in_,a0,a1,a2,b1,b2] Nothing 1 (Special 0) NoId

-- | Duration of one sample.
--
--  SampleDur [InitialisationRate]
sampleDur :: UGen
sampleDur = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "SampleDur" [] Nothing 1 (Special 0) NoId

-- | Server sample rate.
--
--  SampleRate [InitialisationRate]
sampleRate :: UGen
sampleRate = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "SampleRate" [] Nothing 1 (Special 0) NoId

-- | Remove infinity, NaN, and denormals
--
--  Sanitize [ControlRate,AudioRate] in=0 replace=0;    FILTER: TRUE
sanitize :: UGen -> UGen -> UGen
sanitize in_ replace = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Sanitize" [in_,replace] Nothing 1 (Special 0) NoId

-- | Band limited sawtooth.
--
--  Saw [ControlRate,AudioRate] freq=440
saw :: Rate -> UGen -> UGen
saw rate freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Saw" [freq] Nothing 1 (Special 0) NoId

-- | Schmidt trigger.
--
--  Schmidt [InitialisationRate,ControlRate,AudioRate] in=0 lo=0 hi=1;    FILTER: TRUE
schmidt :: UGen -> UGen -> UGen -> UGen
schmidt in_ lo hi = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Right [0]) "Schmidt" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Select output from an array of inputs.
--
--  Select [InitialisationRate,ControlRate,AudioRate] which=0 *array=0;    MCE=1, FILTER: TRUE
select :: UGen -> UGen -> UGen
select which array = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Right [0,1]) "Select" [which] (Just [array]) 1 (Special 0) NoId

-- | Send a trigger message from the server back to the client.
--
--  SendTrig [ControlRate,AudioRate] in=0 id=0 value=0;    FILTER: TRUE
sendTrig :: UGen -> UGen -> UGen -> UGen
sendTrig in_ id_ value = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "SendTrig" [in_,id_,value] Nothing 0 (Special 0) NoId

-- | Set-reset flip flop.
--
--  SetResetFF [ControlRate,AudioRate] trig=0 reset=0;    FILTER: TRUE
setResetFF :: UGen -> UGen -> UGen
setResetFF trig_ reset = mkUGen Nothing [ControlRate,AudioRate] (Right [0,1]) "SetResetFF" [trig_,reset] Nothing 1 (Special 0) NoId

-- | Wave shaper.
--
--  Shaper [ControlRate,AudioRate] bufnum=0 in=0;    FILTER: TRUE
shaper :: UGen -> UGen -> UGen
shaper bufnum in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "Shaper" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Interpolating sine wavetable oscillator.
--
--  SinOsc [ControlRate,AudioRate] freq=440 phase=0
sinOsc :: Rate -> UGen -> UGen -> UGen
sinOsc rate freq phase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "SinOsc" [freq,phase] Nothing 1 (Special 0) NoId

-- | Feedback FM oscillator
--
--  SinOscFB [ControlRate,AudioRate] freq=440 feedback=0
sinOscFB :: Rate -> UGen -> UGen -> UGen
sinOscFB rate freq feedback = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "SinOscFB" [freq,feedback] Nothing 1 (Special 0) NoId

-- | Slew rate limiter.
--
--  Slew [ControlRate,AudioRate] in=0 up=1 dn=1;    FILTER: TRUE
slew :: UGen -> UGen -> UGen -> UGen
slew in_ up dn = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Slew" [in_,up,dn] Nothing 1 (Special 0) NoId

-- | Slope of signal
--
--  Slope [ControlRate,AudioRate] in=0;    FILTER: TRUE
slope :: UGen -> UGen
slope in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Slope" [in_] Nothing 1 (Special 0) NoId

-- | Spectral centroid
--
--  SpecCentroid [ControlRate] buffer=0
specCentroid :: Rate -> UGen -> UGen
specCentroid rate buffer = mkUGen Nothing [ControlRate] (Left rate) "SpecCentroid" [buffer] Nothing 1 (Special 0) NoId

-- | Spectral Flatness measure
--
--  SpecFlatness [ControlRate] buffer=0
specFlatness :: Rate -> UGen -> UGen
specFlatness rate buffer = mkUGen Nothing [ControlRate] (Left rate) "SpecFlatness" [buffer] Nothing 1 (Special 0) NoId

-- | Find a percentile of FFT magnitude spectrum
--
--  SpecPcile [ControlRate] buffer=0 fraction=0.5 interpolate=0
specPcile :: Rate -> UGen -> UGen -> UGen -> UGen
specPcile rate buffer fraction interpolate = mkUGen Nothing [ControlRate] (Left rate) "SpecPcile" [buffer,fraction,interpolate] Nothing 1 (Special 0) NoId

-- | physical model of resonating spring
--
--  Spring [ControlRate,AudioRate] in=0 spring=1 damp=0
spring :: Rate -> UGen -> UGen -> UGen -> UGen
spring rate in_ spring_ damp = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Spring" [in_,spring_,damp] Nothing 1 (Special 0) NoId

-- | Standard map chaotic generator
--
--  StandardL [AudioRate] freq=22050 k=1 xi=0.5 yi=0
standardL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
standardL rate freq k xi yi = mkUGen Nothing [AudioRate] (Left rate) "StandardL" [freq,k,xi,yi] Nothing 1 (Special 0) NoId

-- | Standard map chaotic generator
--
--  StandardN [AudioRate] freq=22050 k=1 xi=0.5 yi=0
standardN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
standardN rate freq k xi yi = mkUGen Nothing [AudioRate] (Left rate) "StandardN" [freq,k,xi,yi] Nothing 1 (Special 0) NoId

-- | Pulse counter.
--
--  Stepper [ControlRate,AudioRate] trig=0 reset=0 min=0 max=7 step=1 resetval=0;    FILTER: TRUE
stepper :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stepper trig_ reset min_ max_ step resetval = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Stepper" [trig_,reset,min_,max_,step,resetval] Nothing 1 (Special 0) NoId

-- | Stereo real-time convolver with linear interpolation
--
--  StereoConvolution2L [AudioRate] in=0 kernelL=0 kernelR=0 trigger=0 framesize=2048 crossfade=1
stereoConvolution2L :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stereoConvolution2L rate in_ kernelL kernelR trigger framesize crossfade = mkUGen Nothing [AudioRate] (Left rate) "StereoConvolution2L" [in_,kernelL,kernelR,trigger,framesize,crossfade] Nothing 2 (Special 0) NoId

-- | Offset from synth start within one sample.
--
--  SubsampleOffset [InitialisationRate]
subsampleOffset :: UGen
subsampleOffset = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "SubsampleOffset" [] Nothing 1 (Special 0) NoId

-- | Sum three signals
--
--  Sum3 [] in0=0 in1=0 in2=0;    FILTER: TRUE
sum3 :: UGen -> UGen -> UGen -> UGen
sum3 in0 in1 in2 = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Right [0,1,2]) "Sum3" [in0,in1,in2] Nothing 1 (Special 0) NoId

-- | Sum four signals
--
--  Sum4 [] in0=0 in1=0 in2=0 in3=0;    FILTER: TRUE
sum4 :: UGen -> UGen -> UGen -> UGen -> UGen
sum4 in0 in1 in2 in3 = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Right [0,1,2,3]) "Sum4" [in0,in1,in2,in3] Nothing 1 (Special 0) NoId

-- | Triggered linear ramp
--
--  Sweep [ControlRate,AudioRate] trig=0 rate=1;    FILTER: TRUE
sweep :: UGen -> UGen -> UGen
sweep trig_ rate_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Sweep" [trig_,rate_] Nothing 1 (Special 0) NoId

-- | Hard sync sawtooth wave.
--
--  SyncSaw [ControlRate,AudioRate] syncFreq=440 sawFreq=440
syncSaw :: Rate -> UGen -> UGen -> UGen
syncSaw rate syncFreq sawFreq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "SyncSaw" [syncFreq,sawFreq] Nothing 1 (Special 0) NoId

-- | Control rate trigger to audio rate trigger converter
--
--  T2A [AudioRate] in=0 offset=0
t2a :: UGen -> UGen -> UGen
t2a in_ offset = mkUGen Nothing [AudioRate] (Left AudioRate) "T2A" [in_,offset] Nothing 1 (Special 0) NoId

-- | Audio rate trigger to control rate trigger converter
--
--  T2K [ControlRate] in=0
t2k :: UGen -> UGen
t2k in_ = mkUGen Nothing [ControlRate] (Left ControlRate) "T2K" [in_] Nothing 1 (Special 0) NoId

-- | physical model of bouncing object
--
--  TBall [ControlRate,AudioRate] in=0 g=10 damp=0 friction=0.01
tBall :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
tBall rate in_ g damp friction_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "TBall" [in_,g,damp,friction_] Nothing 1 (Special 0) NoId

-- | Trigger delay.
--
--  TDelay [ControlRate,AudioRate] in=0 dur=0.1;    FILTER: TRUE
tDelay :: UGen -> UGen -> UGen
tDelay in_ dur = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "TDelay" [in_,dur] Nothing 1 (Special 0) NoId

-- | Demand results as trigger from demand rate UGens.
--
--  TDuty [ControlRate,AudioRate] dur=1 reset=0 doneAction=0 level=1 gapFirst=0;    REORDERS INPUTS: [0,1,3,2,4], ENUMERATION INPUTS: 2=DoneAction
tDuty :: Rate -> UGen -> UGen -> DoneAction UGen -> UGen -> UGen -> UGen
tDuty rate dur reset doneAction level gapFirst = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "TDuty" [dur,reset,(from_done_action doneAction),level,gapFirst] Nothing 1 (Special 0) NoId

-- | Triggered exponential random number generator.
--
--  TExpRand [ControlRate,AudioRate] lo=0.01 hi=1 trig=0;    FILTER: TRUE, NONDET
tExpRandId :: ID a => a -> UGen -> UGen -> UGen -> UGen
tExpRandId z lo hi trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [2]) "TExpRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of TExpRand.
tExpRandM :: UId m => UGen -> UGen -> UGen -> m UGen
tExpRandM = liftUId3 tExpRandId

-- | Unsafe variant of TExpRand.
tExpRand ::  UGen -> UGen -> UGen -> UGen
tExpRand = liftUnsafe3 tExpRandM

-- | Buffer granulator.
--
--  TGrains [AudioRate] trigger=0 bufnum=0 rate=1 centerPos=0 dur=0.1 pan=0 amp=0.1 interp=4;    NC INPUT: True
tGrains :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains numChannels trigger bufnum rate_ centerPos dur pan amp interp = mkUGen Nothing [AudioRate] (Left AudioRate) "TGrains" [trigger,bufnum,rate_,centerPos,dur,pan,amp,interp] Nothing numChannels (Special 0) NoId

-- | Triggered integer random number generator.
--
--  TIRand [ControlRate,AudioRate] lo=0 hi=127 trig=0;    FILTER: TRUE, NONDET
tiRandId :: ID a => a -> UGen -> UGen -> UGen -> UGen
tiRandId z lo hi trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [2]) "TIRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Tirand.
tiRandM :: UId m => UGen -> UGen -> UGen -> m UGen
tiRandM = liftUId3 tiRandId

-- | Unsafe variant of Tirand.
tiRand ::  UGen -> UGen -> UGen -> UGen
tiRand = liftUnsafe3 tiRandM

-- | Triggered random number generator.
--
--  TRand [ControlRate,AudioRate] lo=0 hi=1 trig=0;    FILTER: TRUE, NONDET
tRandId :: ID a => a -> UGen -> UGen -> UGen -> UGen
tRandId z lo hi trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [2]) "TRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of TRand.
tRandM :: UId m => UGen -> UGen -> UGen -> m UGen
tRandM = liftUId3 tRandId

-- | Unsafe variant of TRand.
tRand ::  UGen -> UGen -> UGen -> UGen
tRand = liftUnsafe3 tRandM

-- | Triggered windex.
--
--  TWindex [ControlRate,AudioRate] in=0 normalize=0 *array=0;    MCE=1, FILTER: TRUE, REORDERS INPUTS: [0,2,1], NONDET
tWindexId :: ID a => a -> UGen -> UGen -> UGen -> UGen
tWindexId z in_ normalize array = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "TWindex" [in_,normalize] (Just [array]) 1 (Special 0) (toUId z)

-- | Monad variant of TWindex.
tWindexM :: UId m => UGen -> UGen -> UGen -> m UGen
tWindexM = liftUId3 tWindexId

-- | Unsafe variant of TWindex.
tWindex ::  UGen -> UGen -> UGen -> UGen
tWindex = liftUnsafe3 tWindexM

-- | Returns time since last triggered.
--
--  Timer [ControlRate,AudioRate] trig=0;    FILTER: TRUE
timer :: UGen -> UGen
timer trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Timer" [trig_] Nothing 1 (Special 0) NoId

-- | Toggle flip flop.
--
--  ToggleFF [ControlRate,AudioRate] trig=0;    FILTER: TRUE
toggleFF :: UGen -> UGen
toggleFF trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "ToggleFF" [trig_] Nothing 1 (Special 0) NoId

-- | Timed trigger.
--
--  Trig [ControlRate,AudioRate] in=0 dur=0.1;    FILTER: TRUE
trig :: UGen -> UGen -> UGen
trig in_ dur = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Trig" [in_,dur] Nothing 1 (Special 0) NoId

-- | Timed trigger.
--
--  Trig1 [ControlRate,AudioRate] in=0 dur=0.1;    FILTER: TRUE
trig1 :: UGen -> UGen -> UGen
trig1 in_ dur = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Trig1" [in_,dur] Nothing 1 (Special 0) NoId

-- | Two pole filter.
--
--  TwoPole [ControlRate,AudioRate] in=0 freq=440 radius=0.8;    FILTER: TRUE
twoPole :: UGen -> UGen -> UGen -> UGen
twoPole in_ freq radius = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "TwoPole" [in_,freq,radius] Nothing 1 (Special 0) NoId

-- | Two zero filter.
--
--  TwoZero [ControlRate,AudioRate] in=0 freq=440 radius=0.8;    FILTER: TRUE
twoZero :: UGen -> UGen -> UGen -> UGen
twoZero in_ freq radius = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "TwoZero" [in_,freq,radius] Nothing 1 (Special 0) NoId

-- | Stream in audio from a file, with variable rate
--
--  VDiskIn [AudioRate] bufnum=0 rate=1 loop=0 sendID=0;    NC INPUT: True, ENUMERATION INPUTS: 2=Loop
vDiskIn :: Int -> UGen -> UGen -> Loop UGen -> UGen -> UGen
vDiskIn numChannels bufnum rate_ loop sendID = mkUGen Nothing [AudioRate] (Left AudioRate) "VDiskIn" [bufnum,rate_,(from_loop loop),sendID] Nothing numChannels (Special 0) NoId

-- | Variable wavetable oscillator.
--
--  VOsc [ControlRate,AudioRate] bufpos=0 freq=440 phase=0
vOsc :: Rate -> UGen -> UGen -> UGen -> UGen
vOsc rate bufpos freq phase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "VOsc" [bufpos,freq,phase] Nothing 1 (Special 0) NoId

-- | Three variable wavetable oscillators.
--
--  VOsc3 [ControlRate,AudioRate] bufpos=0 freq1=110 freq2=220 freq3=440
vOsc3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vOsc3 rate bufpos freq1 freq2 freq3 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "VOsc3" [bufpos,freq1,freq2,freq3] Nothing 1 (Special 0) NoId

-- | Variable shaped lag
--
--  VarLag [ControlRate,AudioRate] in=0 time=0.1 curvature=0 warp=5 start=0;    FILTER: TRUE
varLag :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
varLag in_ time curvature warp start = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "VarLag" [in_,time,curvature,warp,start] Nothing 1 (Special 0) NoId

-- | Variable duty saw
--
--  VarSaw [ControlRate,AudioRate] freq=440 iphase=0 width=0.5
varSaw :: Rate -> UGen -> UGen -> UGen -> UGen
varSaw rate freq iphase width = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "VarSaw" [freq,iphase,width] Nothing 1 (Special 0) NoId

-- | The Vibrato oscillator models a slow frequency modulation.
--
--  Vibrato [ControlRate,AudioRate] freq=440 rate=6 depth=0.02 delay=0 onset=0 rateVariation=0.04 depthVariation=0.1 iphase=0 trig=0;    NONDET
vibratoId :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
vibratoId z rate freq rate_ depth delay onset rateVariation depthVariation iphase trig_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Vibrato" [freq,rate_,depth,delay,onset,rateVariation,depthVariation,iphase,trig_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Vibrato.
vibratoM :: UId m => Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> m UGen
vibratoM = liftUId10 vibratoId

-- | Unsafe variant of Vibrato.
vibrato ::  Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
vibrato = liftUnsafe10 vibratoM

-- | Warp a buffer with a time pointer
--
--  Warp1 [AudioRate] bufnum=0 pointer=0 freqScale=1 windowSize=0.2 envbufnum=-1 overlaps=8 windowRandRatio=0 interp=1;    NC INPUT: True
warp1 :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
warp1 numChannels bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp = mkUGen Nothing [AudioRate] (Left AudioRate) "Warp1" [bufnum,pointer,freqScale,windowSize,envbufnum,overlaps,windowRandRatio,interp] Nothing numChannels (Special 0) NoId

-- | White noise.
--
--  WhiteNoise [ControlRate,AudioRate] ;    NONDET
whiteNoiseId :: ID a => a -> Rate -> UGen
whiteNoiseId z rate = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "WhiteNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of WhiteNoise.
whiteNoiseM :: UId m => Rate -> m UGen
whiteNoiseM = liftUId1 whiteNoiseId

-- | Unsafe variant of WhiteNoise.
whiteNoise ::  Rate -> UGen
whiteNoise = liftUnsafe1 whiteNoiseM

-- | Wrap a signal outside given thresholds.
--
--  Wrap [InitialisationRate,ControlRate,AudioRate] in=0 lo=0 hi=1;    FILTER: TRUE
wrap :: UGen -> UGen -> UGen -> UGen
wrap in_ lo hi = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Right [0]) "Wrap" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Index into a table with a signal.
--
--  WrapIndex [ControlRate,AudioRate] bufnum=0 in=0;    FILTER: TRUE
wrapIndex :: UGen -> UGen -> UGen
wrapIndex bufnum in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [1]) "WrapIndex" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Equal power two channel cross fade.
--
--  XFade2 [ControlRate,AudioRate] inA=0 inB=0 pan=0 level=1;    FILTER: TRUE
xFade2 :: UGen -> UGen -> UGen -> UGen -> UGen
xFade2 inA inB pan level = mkUGen Nothing [ControlRate,AudioRate] (Right [0,1]) "XFade2" [inA,inB,pan,level] Nothing 1 (Special 0) NoId

-- | Exponential line generator.
--
--  XLine [ControlRate,AudioRate] start=1 end=2 dur=1 doneAction=0;    ENUMERATION INPUTS: 3=DoneAction
xLine :: Rate -> UGen -> UGen -> UGen -> DoneAction UGen -> UGen
xLine rate start end dur doneAction = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "XLine" [start,end,dur,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Send signal to a bus, crossfading with previous contents.
--
--  XOut [ControlRate,AudioRate] bus=0 xfade=0 *channelsArray=0;    MCE=1, FILTER: TRUE
xOut :: UGen -> UGen -> UGen -> UGen
xOut bus xfade input = mkUGen Nothing [ControlRate,AudioRate] (Right [2]) "XOut" [bus,xfade] (Just [input]) 0 (Special 0) NoId

-- | Zero crossing frequency follower
--
--  ZeroCrossing [ControlRate,AudioRate] in=0;    FILTER: TRUE
zeroCrossing :: UGen -> UGen
zeroCrossing in_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "ZeroCrossing" [in_] Nothing 1 (Special 0) NoId

-- | LocalBuf count
--
--  MaxLocalBufs [InitialisationRate,ControlRate] count=0
maxLocalBufs :: UGen -> UGen
maxLocalBufs count = mkUGen Nothing [InitialisationRate,ControlRate] (Left ControlRate) "MaxLocalBufs" [count] Nothing 1 (Special 0) NoId

-- | Multiply add
--
--  MulAdd [InitialisationRate,ControlRate,AudioRate] in=0 mul=0 add=0;    FILTER: TRUE
mulAdd :: UGen -> UGen -> UGen -> UGen
mulAdd in_ mul add = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate] (Right [0,1,2]) "MulAdd" [in_,mul,add] Nothing 1 (Special 0) NoId

-- | Set local buffer
--
--  SetBuf [InitialisationRate] buf=0 offset=0 length=0 *array=0;    MCE=1, REORDERS INPUTS: [0,1,2,3]
setBuf :: UGen -> UGen -> UGen -> UGen -> UGen
setBuf buf offset length_ array = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "SetBuf" [buf,offset,length_] (Just [array]) 1 (Special 0) NoId
