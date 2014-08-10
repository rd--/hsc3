module Sound.SC3.UGen.Bindings.DB where

import Sound.SC3.UGen.Envelope
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Audio to control rate converter.
a2K :: UGen -> UGen
a2K in_ = mkUGen Nothing [KR] (Left KR) "A2K" [in_] Nothing 1 (Special 0) NoId

-- | FIXME: APF purpose.
apf :: UGen -> UGen -> UGen -> UGen
apf in_ freq radius = mkUGen Nothing [KR,AR] (Right [0]) "APF" [in_,freq,radius] Nothing 1 (Special 0) NoId

-- | All pass delay line with cubic interpolation.
allpassC :: UGen -> UGen -> UGen -> UGen -> UGen
allpassC in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "AllpassC" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | All pass delay line with linear interpolation.
allpassL :: UGen -> UGen -> UGen -> UGen -> UGen
allpassL in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "AllpassL" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | All pass delay line with no interpolation.
allpassN :: UGen -> UGen -> UGen -> UGen -> UGen
allpassN in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "AllpassN" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Basic psychoacoustic amplitude compensation.
ampComp :: Rate -> UGen -> UGen -> UGen -> UGen
ampComp rate freq root exp_ = mkUGen Nothing [IR,KR,AR] (Left rate) "AmpComp" [freq,root,exp_] Nothing 1 (Special 0) NoId

-- | Basic psychoacoustic amplitude compensation (ANSI A-weighting curve).
ampCompA :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
ampCompA rate freq root minAmp rootAmp = mkUGen Nothing [IR,KR,AR] (Left rate) "AmpCompA" [freq,root,minAmp,rootAmp] Nothing 1 (Special 0) NoId

-- | Amplitude follower
amplitude :: Rate -> UGen -> UGen -> UGen -> UGen
amplitude rate in_ attackTime releaseTime = mkUGen Nothing [KR,AR] (Left rate) "Amplitude" [in_,attackTime,releaseTime] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
audioControl :: Rate -> UGen -> UGen
audioControl rate values = mkUGen Nothing [AR] (Left rate) "AudioControl" [values] Nothing 1 (Special 0) NoId

-- | All Pass Filter
bAllPass :: UGen -> UGen -> UGen -> UGen
bAllPass in_ freq rq = mkUGen Nothing [AR] (Right [0]) "BAllPass" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Band Pass Filter
bBandPass :: UGen -> UGen -> UGen -> UGen
bBandPass in_ freq bw = mkUGen Nothing [AR] (Right [0]) "BBandPass" [in_,freq,bw] Nothing 1 (Special 0) NoId

-- | Band reject filter
bBandStop :: UGen -> UGen -> UGen -> UGen
bBandStop in_ freq bw = mkUGen Nothing [AR] (Right [0]) "BBandStop" [in_,freq,bw] Nothing 1 (Special 0) NoId

-- | 12db/oct rolloff - 2nd order resonant  Hi Pass Filter
bHiPass :: UGen -> UGen -> UGen -> UGen
bHiPass in_ freq rq = mkUGen Nothing [AR] (Right [0]) "BHiPass" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Hi Shelf
bHiShelf :: UGen -> UGen -> UGen -> UGen -> UGen
bHiShelf in_ freq rs db = mkUGen Nothing [AR] (Right [0]) "BHiShelf" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | 12db/oct rolloff - 2nd order resonant Low Pass Filter
bLowPass :: UGen -> UGen -> UGen -> UGen
bLowPass in_ freq rq = mkUGen Nothing [AR] (Right [0]) "BLowPass" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Low Shelf
bLowShelf :: UGen -> UGen -> UGen -> UGen -> UGen
bLowShelf in_ freq rs db = mkUGen Nothing [AR] (Right [0]) "BLowShelf" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | 2nd order Butterworth bandpass filter.
bpf :: UGen -> UGen -> UGen -> UGen
bpf in_ freq rq = mkUGen Nothing [KR,AR] (Right [0]) "BPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Two zero fixed midpass.
bpz2 :: UGen -> UGen
bpz2 in_ = mkUGen Nothing [KR,AR] (Right [0]) "BPZ2" [in_] Nothing 1 (Special 0) NoId

-- | Parametric equalizer
bPeakEQ :: UGen -> UGen -> UGen -> UGen -> UGen
bPeakEQ in_ freq rq db = mkUGen Nothing [AR] (Right [0]) "BPeakEQ" [in_,freq,rq,db] Nothing 1 (Special 0) NoId

-- | 2nd order Butterworth band reject filter.
brf :: UGen -> UGen -> UGen -> UGen
brf in_ freq rq = mkUGen Nothing [KR,AR] (Right [0]) "BRF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Two zero fixed midcut.
bRZ2 :: UGen -> UGen
bRZ2 in_ = mkUGen Nothing [KR,AR] (Right [0]) "BRZ2" [in_] Nothing 1 (Special 0) NoId

-- | Stereo signal balancer
balance2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
balance2 rate left right pos level = mkUGen Nothing [KR,AR] (Left rate) "Balance2" [left,right,pos,level] Nothing 2 (Special 0) NoId

-- | physical model of bouncing object
ball :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
ball rate in_ g damp friction = mkUGen Nothing [KR,AR] (Left rate) "Ball" [in_,g,damp,friction] Nothing 1 (Special 0) NoId

-- | Autocorrelation beat tracker
beatTrack :: Rate -> UGen -> UGen -> UGen
beatTrack rate chain lock = mkUGen Nothing [KR] (Left rate) "BeatTrack" [chain,lock] Nothing 1 (Special 0) NoId

-- | Template matching beat tracker
beatTrack2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
beatTrack2 rate busindex numfeatures windowsize phaseaccuracy lock weightingscheme = mkUGen Nothing [KR] (Left rate) "BeatTrack2" [busindex,numfeatures,windowsize,phaseaccuracy,lock,weightingscheme] Nothing 6 (Special 0) NoId

-- | 2D Ambisonic B-format panner.
biPanB2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
biPanB2 rate inA inB azimuth gain = mkUGen Nothing [KR,AR] (Left rate) "BiPanB2" [inA,inB,azimuth,gain] Nothing 3 (Special 0) NoId

-- | Apply a binary operation to the values of an input UGen
binaryOpUGen :: UGen -> UGen -> UGen
binaryOpUGen a b = mkUGen Nothing [IR,KR,AR,DR] (Right [0,1]) "BinaryOpUGen" [a,b] Nothing 1 (Special 0) NoId

-- | Band limited impulse oscillator.
blip :: Rate -> UGen -> UGen -> UGen
blip rate freq numharm = mkUGen Nothing [KR,AR] (Left rate) "Blip" [freq,numharm] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
blockSize :: Rate -> UGen
blockSize rate = mkUGen Nothing [IR] (Left rate) "BlockSize" [] Nothing 1 (Special 0) NoId

-- | Brown Noise.
brownNoise :: ID a => a -> Rate -> UGen
brownNoise z rate = mkUGen Nothing [KR,AR] (Left rate) "BrownNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Buffer based all pass delay line with cubic interpolation.
bufAllpassC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassC rate buf in_ delaytime decaytime = mkUGen Nothing [AR] (Left rate) "BufAllpassC" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based all pass delay line with linear interpolation.
bufAllpassL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassL rate buf in_ delaytime decaytime = mkUGen Nothing [AR] (Left rate) "BufAllpassL" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based all pass delay line with no interpolation.
bufAllpassN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassN rate buf in_ delaytime decaytime = mkUGen Nothing [AR] (Left rate) "BufAllpassN" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Current number of channels of soundfile in buffer.
bufChannels :: Rate -> UGen -> UGen
bufChannels rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufChannels" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer based comb delay line with cubic interpolation.
bufCombC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
bufCombC rate buf in_ delaytime decaytime = mkUGen Nothing [AR] (Left rate) "BufCombC" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based comb delay line with linear interpolation.
bufCombL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
bufCombL rate buf in_ delaytime decaytime = mkUGen Nothing [AR] (Left rate) "BufCombL" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based comb delay line with no interpolation.
bufCombN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
bufCombN rate buf in_ delaytime decaytime = mkUGen Nothing [AR] (Left rate) "BufCombN" [buf,in_,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Buffer based simple delay line with cubic interpolation.
bufDelayC :: Rate -> UGen -> UGen -> UGen -> UGen
bufDelayC rate buf in_ delaytime = mkUGen Nothing [KR,AR] (Left rate) "BufDelayC" [buf,in_,delaytime] Nothing 1 (Special 0) NoId

-- | Buffer based simple delay line with linear interpolation.
bufDelayL :: Rate -> UGen -> UGen -> UGen -> UGen
bufDelayL rate buf in_ delaytime = mkUGen Nothing [KR,AR] (Left rate) "BufDelayL" [buf,in_,delaytime] Nothing 1 (Special 0) NoId

-- | Buffer based simple delay line with no interpolation.
bufDelayN :: Rate -> UGen -> UGen -> UGen -> UGen
bufDelayN rate buf in_ delaytime = mkUGen Nothing [KR,AR] (Left rate) "BufDelayN" [buf,in_,delaytime] Nothing 1 (Special 0) NoId

-- | Current duration of soundfile in buffer.
bufDur :: Rate -> UGen -> UGen
bufDur rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufDur" [bufnum] Nothing 1 (Special 0) NoId

-- | Current number of frames allocated in the buffer.
bufFrames :: Rate -> UGen -> UGen
bufFrames rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufFrames" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer rate scaling in respect to server samplerate.
bufRateScale :: Rate -> UGen -> UGen
bufRateScale rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufRateScale" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer reading oscillator.
bufRd :: Int -> Rate -> UGen -> UGen -> Loop -> Interpolation -> UGen
bufRd numChannels rate bufnum phase loop interpolation = mkUGen Nothing [KR,AR] (Left rate) "BufRd" [bufnum,phase,(from_loop loop),(from_interpolation interpolation)] Nothing numChannels (Special 0) NoId

-- | Buffer sample rate.
bufSampleRate :: Rate -> UGen -> UGen
bufSampleRate rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufSampleRate" [bufnum] Nothing 1 (Special 0) NoId

-- | Current number of samples in buffer.
bufSamples :: Rate -> UGen -> UGen
bufSamples rate bufnum = mkUGen Nothing [IR,KR] (Left rate) "BufSamples" [bufnum] Nothing 1 (Special 0) NoId

-- | Buffer writing oscillator.
bufWr :: UGen -> UGen -> Loop -> UGen -> UGen
bufWr bufnum phase loop inputArray = mkUGen Nothing [KR,AR] (Right [3]) "BufWr" [bufnum,phase,(from_loop loop)] (Just inputArray) 1 (Special 0) NoId

-- | Chorusing wavetable oscillator.
cOsc :: Rate -> UGen -> UGen -> UGen -> UGen
cOsc rate bufnum freq beats = mkUGen Nothing [KR,AR] (Left rate) "COsc" [bufnum,freq,beats] Nothing 1 (Special 0) NoId

-- | Test for infinity, not-a-number, and denormals
checkBadValues :: UGen -> UGen -> UGen -> UGen
checkBadValues in_ id_ post = mkUGen Nothing [KR,AR] (Right [0]) "CheckBadValues" [in_,id_,post] Nothing 1 (Special 0) NoId

-- | Clip a signal outside given thresholds.
clip :: UGen -> UGen -> UGen -> UGen
clip in_ lo hi = mkUGen Nothing [IR,KR,AR] (Right [0]) "Clip" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Clip Noise.
clipNoise :: ID a => a -> Rate -> UGen
clipNoise z rate = mkUGen Nothing [KR,AR] (Left rate) "ClipNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Statistical gate.
coinGate :: ID a => a -> UGen -> UGen -> UGen
coinGate z prob in_ = mkUGen Nothing [KR,AR] (Right [1]) "CoinGate" [prob,in_] Nothing 1 (Special 0) (toUId z)

-- | Comb delay line with cubic interpolation.
combC :: UGen -> UGen -> UGen -> UGen -> UGen
combC in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "CombC" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Comb delay line with linear interpolation.
combL :: UGen -> UGen -> UGen -> UGen -> UGen
combL in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "CombL" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Comb delay line with no interpolation.
combN :: UGen -> UGen -> UGen -> UGen -> UGen
combN in_ maxdelaytime delaytime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "CombN" [in_,maxdelaytime,delaytime,decaytime] Nothing 1 (Special 0) NoId

-- | Compressor, expander, limiter, gate, ducker
compander :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
compander in_ control_ thresh slopeBelow slopeAbove clampTime relaxTime = mkUGen Nothing [AR] (Right [0]) "Compander" [in_,control_,thresh,slopeBelow,slopeAbove,clampTime,relaxTime] Nothing 1 (Special 0) NoId

-- | Compressor, expander, limiter, gate, ducker.
companderD :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
companderD rate in_ thresh slopeBelow slopeAbove clampTime relaxTime = mkUGen Nothing [AR] (Left rate) "CompanderD" [in_,thresh,slopeBelow,slopeAbove,clampTime,relaxTime] Nothing 1 (Special 0) NoId

-- | Duration of one block
controlDur :: UGen
controlDur = mkUGen Nothing [IR] (Left IR) "ControlDur" [] Nothing 1 (Special 0) NoId

-- | Server control rate.
controlRate :: UGen
controlRate = mkUGen Nothing [IR] (Left IR) "ControlRate" [] Nothing 1 (Special 0) NoId

-- | Real-time convolver.
convolution :: Rate -> UGen -> UGen -> UGen -> UGen
convolution rate in_ kernel framesize = mkUGen Nothing [AR] (Left rate) "Convolution" [in_,kernel,framesize] Nothing 1 (Special 0) NoId

-- | Real-time fixed kernel convolver.
convolution2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
convolution2 rate in_ kernel trigger framesize = mkUGen Nothing [AR] (Left rate) "Convolution2" [in_,kernel,trigger,framesize] Nothing 1 (Special 0) NoId

-- | Real-time convolver with linear interpolation
convolution2L :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
convolution2L rate in_ kernel trigger framesize crossfade = mkUGen Nothing [AR] (Left rate) "Convolution2L" [in_,kernel,trigger,framesize,crossfade] Nothing 1 (Special 0) NoId

-- | Time based convolver.
convolution3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
convolution3 rate in_ kernel trigger framesize = mkUGen Nothing [KR,AR] (Left rate) "Convolution3" [in_,kernel,trigger,framesize] Nothing 1 (Special 0) NoId

-- | Chaotic noise function.
crackle :: Rate -> UGen -> UGen
crackle rate chaosParam = mkUGen Nothing [KR,AR] (Left rate) "Crackle" [chaosParam] Nothing 1 (Special 0) NoId

-- | Cusp map chaotic generator
cuspL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
cuspL rate freq a b xi = mkUGen Nothing [AR] (Left rate) "CuspL" [freq,a,b,xi] Nothing 1 (Special 0) NoId

-- | Cusp map chaotic generator
cuspN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
cuspN rate freq a b xi = mkUGen Nothing [AR] (Left rate) "CuspN" [freq,a,b,xi] Nothing 1 (Special 0) NoId

-- | Create a constant amplitude signal
dc :: Rate -> UGen -> UGen
dc rate in_ = mkUGen Nothing [KR,AR] (Left rate) "DC" [in_] Nothing 1 (Special 0) NoId

-- | Demand rate brownian movement generator.
dbrown :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
dbrown z length_ lo hi step = mkUGen Nothing [DR] (Left DR) "Dbrown" [length_,lo,hi,step] Nothing 1 (Special 0) (toUId z)

-- | Buffer read demand ugen
dbufrd :: ID a => a -> UGen -> UGen -> Loop -> UGen
dbufrd z bufnum phase loop = mkUGen Nothing [DR] (Left DR) "Dbufrd" [bufnum,phase,(from_loop loop)] Nothing 1 (Special 0) (toUId z)

-- | Buffer write demand ugen
dbufwr :: ID a => a -> UGen -> UGen -> UGen -> Loop -> UGen
dbufwr z bufnum phase loop input = mkUGen Nothing [DR] (Left DR) "Dbufwr" [bufnum,phase,loop,(from_loop input)] Nothing 1 (Special 0) (toUId z)

-- | Exponential decay
decay :: UGen -> UGen -> UGen
decay in_ decayTime = mkUGen Nothing [KR,AR] (Right [0]) "Decay" [in_,decayTime] Nothing 1 (Special 0) NoId

-- | Exponential decay
decay2 :: UGen -> UGen -> UGen -> UGen
decay2 in_ attackTime decayTime = mkUGen Nothing [KR,AR] (Right [0]) "Decay2" [in_,attackTime,decayTime] Nothing 1 (Special 0) NoId

-- | 2D Ambisonic B-format decoder.
decodeB2 :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen
decodeB2 numChannels rate w x y orientation = mkUGen Nothing [KR,AR] (Left rate) "DecodeB2" [w,x,y,orientation] Nothing numChannels (Special 0) NoId

-- | Convert signal to modal pitch.
degreeToKey :: UGen -> UGen -> UGen -> UGen
degreeToKey bufnum in_ octave = mkUGen Nothing [KR,AR] (Right [1]) "DegreeToKey" [bufnum,in_,octave] Nothing 1 (Special 0) NoId

-- | Tap a delay line from a DelTapWr UGen
delTapRd :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
delTapRd rate buffer phase delTime interp = mkUGen Nothing [KR,AR] (Left rate) "DelTapRd" [buffer,phase,delTime,interp] Nothing 1 (Special 0) NoId

-- | Write to a buffer for a DelTapRd UGen
delTapWr :: Rate -> UGen -> UGen -> UGen
delTapWr rate buffer in_ = mkUGen Nothing [KR,AR] (Left rate) "DelTapWr" [buffer,in_] Nothing 1 (Special 0) NoId

-- | Single sample delay.
delay1 :: UGen -> UGen
delay1 in_ = mkUGen Nothing [KR,AR] (Right [0]) "Delay1" [in_] Nothing 1 (Special 0) NoId

-- | Two sample delay.
delay2 :: UGen -> UGen
delay2 in_ = mkUGen Nothing [KR,AR] (Right [0]) "Delay2" [in_] Nothing 1 (Special 0) NoId

-- | Simple delay line with cubic interpolation.
delayC :: UGen -> UGen -> UGen -> UGen
delayC in_ maxdelaytime delaytime = mkUGen Nothing [KR,AR] (Right [0]) "DelayC" [in_,maxdelaytime,delaytime] Nothing 1 (Special 0) NoId

-- | Simple delay line with linear interpolation.
delayL :: UGen -> UGen -> UGen -> UGen
delayL in_ maxdelaytime delaytime = mkUGen Nothing [KR,AR] (Right [0]) "DelayL" [in_,maxdelaytime,delaytime] Nothing 1 (Special 0) NoId

-- | Simple delay line with no interpolation.
delayN :: UGen -> UGen -> UGen -> UGen
delayN in_ maxdelaytime delaytime = mkUGen Nothing [KR,AR] (Right [0]) "DelayN" [in_,maxdelaytime,delaytime] Nothing 1 (Special 0) NoId

-- | Demand results from demand rate UGens.
demand :: UGen -> UGen -> UGen -> UGen
demand trig_ reset demandUGens = mkUGen Nothing [KR,AR] (Right [0]) "Demand" [trig_,reset] (Just demandUGens) (length (mceChannels demandUGens) + 0) (Special 0) NoId

-- | Demand rate envelope generator
demandEnvGen :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> DoneAction -> UGen
demandEnvGen rate level dur shape curve gate_ reset levelScale levelBias timeScale doneAction = mkUGen Nothing [KR,AR] (Left rate) "DemandEnvGen" [level,dur,shape,curve,gate_,reset,levelScale,levelBias,timeScale,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Search a buffer for a value
detectIndex :: UGen -> UGen -> UGen
detectIndex bufnum in_ = mkUGen Nothing [KR,AR] (Right [1]) "DetectIndex" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | When input falls below a threshhold, evaluate doneAction.
detectSilence :: UGen -> UGen -> UGen -> DoneAction -> UGen
detectSilence in_ amp time doneAction = mkUGen Nothing [KR,AR] (Right [0]) "DetectSilence" [in_,amp,time,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Demand rate geometric series UGen.
dgeom :: ID a => a -> UGen -> UGen -> UGen -> UGen
dgeom z length_ start grow = mkUGen Nothing [DR] (Left DR) "Dgeom" [length_,start,grow] Nothing 1 (Special 0) (toUId z)

-- | Demand rate brownian movement generator.
dibrown :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
dibrown z length_ lo hi step = mkUGen Nothing [DR] (Left DR) "Dibrown" [length_,lo,hi,step] Nothing 1 (Special 0) (toUId z)

-- | Stream in audio from a file.
diskIn :: Int -> UGen -> Loop -> UGen
diskIn numChannels bufnum loop = mkUGen Nothing [AR] (Left AR) "DiskIn" [bufnum,(from_loop loop)] Nothing numChannels (Special 0) NoId

-- | Record to a soundfile to disk.
diskOut :: UGen -> UGen -> UGen
diskOut bufnum input = mkUGen Nothing [AR] (Left AR) "DiskOut" [bufnum] (Just input) 1 (Special 0) NoId

-- | Demand rate white noise random generator.
diwhite :: ID a => a -> UGen -> UGen -> UGen -> UGen
diwhite z length_ lo hi = mkUGen Nothing [DR] (Left DR) "Diwhite" [length_,lo,hi] Nothing 1 (Special 0) (toUId z)

-- | (Undocumented class)
donce :: ID a => a -> UGen -> UGen
donce z in_ = mkUGen Nothing [DR] (Left DR) "Donce" [in_] Nothing 1 (Special 0) (toUId z)

-- | Monitors another UGen to see when it is finished
done :: Rate -> UGen -> UGen
done rate src = mkUGen Nothing [KR] (Left rate) "Done" [src] Nothing 1 (Special 0) NoId

-- | Print the current output value of a demand rate UGen
dpoll :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
dpoll z in_ label_ run trigid = mkUGen Nothing [DR] (Left DR) "Dpoll" [in_,label_,run,trigid] Nothing 1 (Special 0) (toUId z)

-- | Demand rate random sequence generator.
drand :: ID a => a -> UGen -> UGen -> UGen
drand z repeats list_ = mkUGen Nothing [DR] (Left DR) "Drand" [repeats] (Just list_) 1 (Special 0) (toUId z)

-- | demand rate reset
dreset :: ID a => a -> UGen -> UGen -> UGen
dreset z in_ reset = mkUGen Nothing [DR] (Left DR) "Dreset" [in_,reset] Nothing 1 (Special 0) (toUId z)

-- | Demand rate sequence generator.
dseq :: ID a => a -> UGen -> UGen -> UGen
dseq z repeats list_ = mkUGen Nothing [DR] (Left DR) "Dseq" [repeats] (Just list_) 1 (Special 0) (toUId z)

-- | Demand rate sequence generator.
dser :: ID a => a -> UGen -> UGen -> UGen
dser z repeats list_ = mkUGen Nothing [DR] (Left DR) "Dser" [repeats] (Just list_) 1 (Special 0) (toUId z)

-- | Demand rate arithmetic series UGen.
dseries :: ID a => a -> UGen -> UGen -> UGen -> UGen
dseries z length_ start step = mkUGen Nothing [DR] (Left DR) "Dseries" [length_,start,step] Nothing 1 (Special 0) (toUId z)

-- | Demand rate random sequence generator
dshuf :: ID a => a -> UGen -> UGen -> UGen
dshuf z repeats list_ = mkUGen Nothing [DR] (Left DR) "Dshuf" [repeats] (Just list_) 1 (Special 0) (toUId z)

-- | Demand rate input replicator
dstutter :: ID a => a -> UGen -> UGen -> UGen
dstutter z n in_ = mkUGen Nothing [DR] (Left DR) "Dstutter" [n,in_] Nothing 1 (Special 0) (toUId z)

-- | Demand rate generator for embedding different inputs
dswitch :: ID a => a -> UGen -> UGen -> UGen
dswitch z index_ list_ = mkUGen Nothing [DR] (Left DR) "Dswitch" [index_] (Just list_) 1 (Special 0) (toUId z)

-- | Demand rate generator for switching between inputs.
dswitch1 :: ID a => a -> UGen -> UGen -> UGen
dswitch1 z index_ list_ = mkUGen Nothing [DR] (Left DR) "Dswitch1" [index_] (Just list_) 1 (Special 0) (toUId z)

-- | Return the same unique series of values for several demand streams
dunique :: ID a => a -> UGen -> UGen -> UGen -> UGen
dunique z source maxBufferSize protected = mkUGen Nothing [DR] (Left DR) "Dunique" [source,maxBufferSize,protected] Nothing 1 (Special 0) (toUId z)

-- | Random impulses.
dust :: ID a => a -> Rate -> UGen -> UGen
dust z rate density = mkUGen Nothing [KR,AR] (Left rate) "Dust" [density] Nothing 1 (Special 0) (toUId z)

-- | Random impulses.
dust2 :: ID a => a -> Rate -> UGen -> UGen
dust2 z rate density = mkUGen Nothing [KR,AR] (Left rate) "Dust2" [density] Nothing 1 (Special 0) (toUId z)

-- | (Undocumented class)
dustR :: Rate -> UGen -> UGen -> UGen
dustR rate iot_min iot_max = mkUGen Nothing [AR] (Left rate) "DustR" [iot_min,iot_max] Nothing 1 (Special 0) NoId

-- | Demand results from demand rate UGens.
duty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen
duty rate dur reset doneAction level = mkUGen Nothing [KR,AR] (Left rate) "Duty" [dur,reset,(from_done_action doneAction),level] Nothing 1 (Special 0) NoId

-- | Demand rate white noise random generator.
dwhite :: ID a => a -> UGen -> UGen -> UGen -> UGen
dwhite z length_ lo hi = mkUGen Nothing [DR] (Left DR) "Dwhite" [length_,lo,hi] Nothing 1 (Special 0) (toUId z)

{-
- | Demand rate weighted random sequence generator
dwrand :: ID a => a -> UGen -> UGen -> UGen -> UGen
dwrand z repeats weights list_ = mkUGen Nothing [DR] (Left DR) "Dwrand" [repeats,weights] (Just list_) 1 (Special 0) (toUId z)
-}

-- | Demand rate random sequence generator.
dxrand :: ID a => a -> UGen -> UGen -> UGen
dxrand z repeats list_ = mkUGen Nothing [DR] (Left DR) "Dxrand" [repeats] (Just list_) 1 (Special 0) (toUId z)

-- | Envelope generator
envGen :: Rate -> UGen -> UGen -> UGen -> UGen -> DoneAction -> Envelope UGen -> UGen
envGen rate gate_ levelScale levelBias timeScale doneAction envelope_ = mkUGen Nothing [KR,AR] (Left rate) "EnvGen" [gate_,levelScale,levelBias,timeScale,(from_done_action doneAction)] (Just (envelope_to_ugen envelope_)) 1 (Special 0) NoId

-- | Exponential single random number generator.
expRand :: ID a => a -> UGen -> UGen -> UGen
expRand z lo hi = mkUGen Nothing [IR] (Right [0,1]) "ExpRand" [lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Feedback sine with chaotic phase indexing
fBSineC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fBSineC rate freq im fb a c xi yi = mkUGen Nothing [AR] (Left rate) "FBSineC" [freq,im,fb,a,c,xi,yi] Nothing 1 (Special 0) NoId

-- | Feedback sine with chaotic phase indexing
fBSineL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fBSineL rate freq im fb a c xi yi = mkUGen Nothing [AR] (Left rate) "FBSineL" [freq,im,fb,a,c,xi,yi] Nothing 1 (Special 0) NoId

-- | Feedback sine with chaotic phase indexing
fBSineN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fBSineN rate freq im fb a c xi yi = mkUGen Nothing [AR] (Left rate) "FBSineN" [freq,im,fb,a,c,xi,yi] Nothing 1 (Special 0) NoId

-- | Fast Fourier Transform
fft :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fft buffer in_ hop wintype active winsize = mkUGen Nothing [KR] (Left KR) "FFT" [buffer,in_,hop,wintype,active,winsize] Nothing 1 (Special 0) NoId

-- | First order filter section.
fos :: UGen -> UGen -> UGen -> UGen -> UGen
fos in_ a0 a1 b1 = mkUGen Nothing [KR,AR] (Right [0]) "FOS" [in_,a0,a1,b1] Nothing 1 (Special 0) NoId

-- | Fast sine oscillator.
fSinOsc :: Rate -> UGen -> UGen -> UGen
fSinOsc rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "FSinOsc" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Fold a signal outside given thresholds.
fold :: UGen -> UGen -> UGen -> UGen
fold in_ lo hi = mkUGen Nothing [IR,KR,AR] (Right [0]) "Fold" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Formant oscillator
formant :: Rate -> UGen -> UGen -> UGen -> UGen
formant rate fundfreq formfreq bwfreq = mkUGen Nothing [AR] (Left rate) "Formant" [fundfreq,formfreq,bwfreq] Nothing 1 (Special 0) NoId

-- | FOF-like filter.
formlet :: UGen -> UGen -> UGen -> UGen -> UGen
formlet in_ freq attacktime decaytime = mkUGen Nothing [KR,AR] (Right [0]) "Formlet" [in_,freq,attacktime,decaytime] Nothing 1 (Special 0) NoId

-- | When triggered, frees a node.
free :: UGen -> UGen -> UGen
free trig_ id_ = mkUGen Nothing [KR] (Right [0]) "Free" [trig_,id_] Nothing 1 (Special 0) NoId

-- | When triggered, free enclosing synth.
freeSelf :: UGen -> UGen
freeSelf in_ = mkUGen Nothing [KR] (Left KR) "FreeSelf" [in_] Nothing 1 (Special 0) NoId

-- | Free the enclosing synth when a UGen is finished
freeSelfWhenDone :: Rate -> UGen -> UGen
freeSelfWhenDone rate src = mkUGen Nothing [KR] (Left rate) "FreeSelfWhenDone" [src] Nothing 1 (Special 0) NoId

-- | A reverb
freeVerb :: UGen -> UGen -> UGen -> UGen -> UGen
freeVerb in_ mix room damp = mkUGen Nothing [AR] (Right [0]) "FreeVerb" [in_,mix,room,damp] Nothing 1 (Special 0) NoId

-- | A two-channel reverb
freeVerb2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
freeVerb2 in_ in2 mix room damp = mkUGen Nothing [AR] (Right [0]) "FreeVerb2" [in_,in2,mix,room,damp] Nothing 2 (Special 0) NoId

-- | Frequency Shifter.
freqShift :: Rate -> UGen -> UGen -> UGen -> UGen
freqShift rate in_ freq phase = mkUGen Nothing [AR] (Left rate) "FreqShift" [in_,freq,phase] Nothing 1 (Special 0) NoId

-- | A two-channel reverb
gVerb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gVerb in_ roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize = mkUGen Nothing [AR] (Right [0]) "GVerb" [in_,roomsize,revtime,damping,inputbw,spread,drylevel,earlyreflevel,taillevel,maxroomsize] Nothing 2 (Special 0) NoId

-- | Gate or hold.
gate :: UGen -> UGen -> UGen
gate in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "Gate" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Gingerbreadman map chaotic generator
gbmanL :: Rate -> UGen -> UGen -> UGen -> UGen
gbmanL rate freq xi yi = mkUGen Nothing [AR] (Left rate) "GbmanL" [freq,xi,yi] Nothing 1 (Special 0) NoId

-- | Gingerbreadman map chaotic generator
gbmanN :: Rate -> UGen -> UGen -> UGen -> UGen
gbmanN rate freq xi yi = mkUGen Nothing [AR] (Left rate) "GbmanN" [freq,xi,yi] Nothing 1 (Special 0) NoId

-- | Dynamic stochastic synthesis generator.
gendy1 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy1 z rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum = mkUGen Nothing [KR,AR] (Left rate) "Gendy1" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) (toUId z)

-- | Dynamic stochastic synthesis generator.
gendy2 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy2 z rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c = mkUGen Nothing [KR,AR] (Left rate) "Gendy2" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum,a,c] Nothing 1 (Special 0) (toUId z)

-- | Dynamic stochastic synthesis generator.
gendy3 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy3 z rate ampdist durdist adparam ddparam freq ampscale durscale initCPs knum = mkUGen Nothing [KR,AR] (Left rate) "Gendy3" [ampdist,durdist,adparam,ddparam,freq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) (toUId z)

-- | Granular synthesis with sound stored in a buffer
grainBuf :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainBuf numChannels trigger dur sndbuf rate_ pos interp pan envbufnum maxGrains = mkUGen Nothing [AR] (Left AR) "GrainBuf" [trigger,dur,sndbuf,rate_,pos,interp,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Granular synthesis with frequency modulated sine tones
grainFM :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainFM numChannels trigger dur carfreq modfreq index_ pan envbufnum maxGrains = mkUGen Nothing [AR] (Left AR) "GrainFM" [trigger,dur,carfreq,modfreq,index_,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Granulate an input signal
grainIn :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainIn numChannels trigger dur in_ pan envbufnum maxGrains = mkUGen Nothing [AR] (Left AR) "GrainIn" [trigger,dur,in_,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Granular synthesis with sine tones
grainSin :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainSin numChannels trigger dur freq pan envbufnum maxGrains = mkUGen Nothing [AR] (Left AR) "GrainSin" [trigger,dur,freq,pan,envbufnum,maxGrains] Nothing numChannels (Special 0) NoId

-- | Gray Noise.
grayNoise :: ID a => a -> Rate -> UGen
grayNoise z rate = mkUGen Nothing [KR,AR] (Left rate) "GrayNoise" [] Nothing 1 (Special 0) (toUId z)

-- | 2nd order Butterworth highpass filter.
hpf :: UGen -> UGen -> UGen
hpf in_ freq = mkUGen Nothing [KR,AR] (Right [0]) "HPF" [in_,freq] Nothing 1 (Special 0) NoId

-- | Two point difference filter
hpz1 :: UGen -> UGen
hpz1 in_ = mkUGen Nothing [KR,AR] (Right [0]) "HPZ1" [in_] Nothing 1 (Special 0) NoId

-- | Two zero fixed midcut.
hPZ2 :: UGen -> UGen
hPZ2 in_ = mkUGen Nothing [KR,AR] (Right [0]) "HPZ2" [in_] Nothing 1 (Special 0) NoId

-- | Randomized value.
hasher :: UGen -> UGen
hasher in_ = mkUGen Nothing [KR,AR] (Right [0]) "Hasher" [in_] Nothing 1 (Special 0) NoId

-- | Henon map chaotic generator
henonC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonC rate freq a b x0 x1 = mkUGen Nothing [AR] (Left rate) "HenonC" [freq,a,b,x0,x1] Nothing 1 (Special 0) NoId

-- | Henon map chaotic generator
henonL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonL rate freq a b x0 x1 = mkUGen Nothing [AR] (Left rate) "HenonL" [freq,a,b,x0,x1] Nothing 1 (Special 0) NoId

-- | Henon map chaotic generator
henonN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonN rate freq a b x0 x1 = mkUGen Nothing [AR] (Left rate) "HenonN" [freq,a,b,x0,x1] Nothing 1 (Special 0) NoId

-- | Applies the Hilbert transform to an input signal.
hilbert :: UGen -> UGen
hilbert in_ = mkUGen Nothing [AR] (Right [0]) "Hilbert" [in_] Nothing 2 (Special 0) NoId

-- | Applies the Hilbert transform to an input signal.
hilbertFIR :: Rate -> UGen -> UGen -> UGen
hilbertFIR rate in_ buffer = mkUGen Nothing [AR] (Left rate) "HilbertFIR" [in_,buffer] Nothing 2 (Special 0) NoId

-- | Envelope generator for polling values from an Env
iEnvGen :: Rate -> UGen -> Envelope UGen -> UGen
iEnvGen rate index_ envelope_ = mkUGen Nothing [KR,AR] (Left rate) "IEnvGen" [index_] (Just (envelope_to_ugen envelope_)) 1 (Special 0) NoId

-- | Inverse Fast Fourier Transform
ifft :: UGen -> UGen -> UGen -> UGen
ifft buffer wintype winsize = mkUGen Nothing [KR,AR] (Left AR) "IFFT" [buffer,wintype,winsize] Nothing 1 (Special 0) NoId

-- | Single integer random number generator.
iRand :: ID a => a -> UGen -> UGen -> UGen
iRand z lo hi = mkUGen Nothing [IR] (Left IR) "IRand" [lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Impulse oscillator.
impulse :: Rate -> UGen -> UGen -> UGen
impulse rate freq phase = mkUGen Nothing [KR,AR] (Left rate) "Impulse" [freq,phase] Nothing 1 (Special 0) NoId

-- | Read a signal from a bus.
in' :: Int -> Rate -> UGen -> UGen
in' numChannels rate bus = mkUGen Nothing [KR,AR] (Left rate) "In" [bus] Nothing numChannels (Special 0) NoId

-- | Read signal from a bus with a current or one cycle old timestamp.
inFeedback :: Int -> UGen -> UGen
inFeedback numChannels bus = mkUGen Nothing [AR] (Left AR) "InFeedback" [bus] Nothing numChannels (Special 0) NoId

-- | Tests if a signal is within a given range.
inRange :: UGen -> UGen -> UGen -> UGen
inRange in_ lo hi = mkUGen Nothing [IR,KR,AR] (Right [0]) "InRange" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Test if a point is within a given rectangle.
inRect :: Rate -> UGen -> UGen -> UGen -> UGen
inRect rate x y rect = mkUGen Nothing [KR,AR] (Left rate) "InRect" [x,y,rect] Nothing 1 (Special 0) NoId

-- | Generate a trigger anytime a bus is set.
inTrig :: Int -> Rate -> UGen -> UGen
inTrig numChannels rate bus = mkUGen Nothing [KR] (Left rate) "InTrig" [bus] Nothing numChannels (Special 0) NoId

-- | Index into a table with a signal
index :: UGen -> UGen -> UGen
index bufnum in_ = mkUGen Nothing [KR,AR] (Right [1]) "Index" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Finds the (lowest) point in the Buffer at which the input signal lies in-between the two values
indexInBetween :: Rate -> UGen -> UGen -> UGen
indexInBetween rate bufnum in_ = mkUGen Nothing [KR,AR] (Left rate) "IndexInBetween" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Index into a table with a signal, linear interpolated
indexL :: Rate -> UGen -> UGen -> UGen
indexL rate bufnum in_ = mkUGen Nothing [KR,AR] (Left rate) "IndexL" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Base class for info ugens
infoUGenBase :: Rate -> UGen
infoUGenBase rate = mkUGen Nothing [IR] (Left rate) "InfoUGenBase" [] Nothing 1 (Special 0) NoId

-- | A leaky integrator.
integrator :: UGen -> UGen -> UGen
integrator in_ coef = mkUGen Nothing [KR,AR] (Right [0]) "Integrator" [in_,coef] Nothing 1 (Special 0) NoId

-- | Control to audio rate converter.
k2A :: UGen -> UGen
k2A in_ = mkUGen Nothing [AR] (Left AR) "K2A" [in_] Nothing 1 (Special 0) NoId

-- | Respond to the state of a key
keyState :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
keyState rate keycode minval maxval lag_ = mkUGen Nothing [KR] (Left rate) "KeyState" [keycode,minval,maxval,lag_] Nothing 1 (Special 0) NoId

-- | Key tracker
keyTrack :: Rate -> UGen -> UGen -> UGen -> UGen
keyTrack rate chain keydecay chromaleak = mkUGen Nothing [KR] (Left rate) "KeyTrack" [chain,keydecay,chromaleak] Nothing 1 (Special 0) NoId

-- | Sine oscillator bank
klang :: Rate -> UGen -> UGen -> UGen -> UGen
klang rate freqscale freqoffset specificationsArrayRef = mkUGen Nothing [AR] (Left rate) "Klang" [freqscale,freqoffset] (Just specificationsArrayRef) 1 (Special 0) NoId

-- | Bank of resonators
klank :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
klank input freqscale freqoffset decayscale specificationsArrayRef = mkUGen Nothing [AR] (Right [0]) "Klank" [input,freqscale,freqoffset,decayscale] (Just specificationsArrayRef) 1 (Special 0) NoId

-- | Clipped noise
lfClipNoise :: ID a => a -> Rate -> UGen -> UGen
lfClipNoise z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFClipNoise" [freq] Nothing 1 (Special 0) (toUId z)

-- | A sine like shape made of two cubic pieces
lfCub :: Rate -> UGen -> UGen -> UGen
lfCub rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "LFCub" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Dynamic clipped noise
lfdClipNoise :: ID a => a -> Rate -> UGen -> UGen
lfdClipNoise z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFDClipNoise" [freq] Nothing 1 (Special 0) (toUId z)

-- | Dynamic step noise
lfdNoise0 :: ID a => a -> Rate -> UGen -> UGen
lfdNoise0 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFDNoise0" [freq] Nothing 1 (Special 0) (toUId z)

-- | Dynamic ramp noise
lfdNoise1 :: ID a => a -> Rate -> UGen -> UGen
lfdNoise1 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFDNoise1" [freq] Nothing 1 (Special 0) (toUId z)

-- | Dynamic cubic noise
lfdNoise3 :: ID a => a -> Rate -> UGen -> UGen
lfdNoise3 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFDNoise3" [freq] Nothing 1 (Special 0) (toUId z)

-- | Gaussian function oscillator
lfGauss :: Rate -> UGen -> UGen -> UGen -> Loop -> DoneAction -> UGen
lfGauss rate duration width iphase loop doneAction = mkUGen Nothing [KR,AR] (Left rate) "LFGauss" [duration,width,iphase,(from_loop loop),(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Step noise
lfNoise0 :: ID a => a -> Rate -> UGen -> UGen
lfNoise0 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFNoise0" [freq] Nothing 1 (Special 0) (toUId z)

-- | Ramp noise
lfNoise1 :: ID a => a -> Rate -> UGen -> UGen
lfNoise1 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFNoise1" [freq] Nothing 1 (Special 0) (toUId z)

-- | Quadratic noise.
lfNoise2 :: ID a => a -> Rate -> UGen -> UGen
lfNoise2 z rate freq = mkUGen Nothing [KR,AR] (Left rate) "LFNoise2" [freq] Nothing 1 (Special 0) (toUId z)

-- | Parabolic oscillator
lfPar :: Rate -> UGen -> UGen -> UGen
lfPar rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "LFPar" [freq,iphase] Nothing 1 (Special 0) NoId

-- | pulse oscillator
lfPulse :: Rate -> UGen -> UGen -> UGen -> UGen
lfPulse rate freq iphase width = mkUGen Nothing [KR,AR] (Left rate) "LFPulse" [freq,iphase,width] Nothing 1 (Special 0) NoId

-- | Sawtooth oscillator
lfSaw :: Rate -> UGen -> UGen -> UGen
lfSaw rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "LFSaw" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Triangle oscillator
lfTri :: Rate -> UGen -> UGen -> UGen
lfTri rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "LFTri" [freq,iphase] Nothing 1 (Special 0) NoId

-- | 2nd order Butterworth lowpass filter
lpf :: UGen -> UGen -> UGen
lpf in_ freq = mkUGen Nothing [KR,AR] (Right [0]) "LPF" [in_,freq] Nothing 1 (Special 0) NoId

-- | Two point average filter
lPZ1 :: UGen -> UGen
lPZ1 in_ = mkUGen Nothing [KR,AR] (Right [0]) "LPZ1" [in_] Nothing 1 (Special 0) NoId

-- | Two zero fixed lowpass
lPZ2 :: UGen -> UGen
lPZ2 in_ = mkUGen Nothing [KR,AR] (Right [0]) "LPZ2" [in_] Nothing 1 (Special 0) NoId

-- | Exponential lag
lag :: UGen -> UGen -> UGen
lag in_ lagTime = mkUGen Nothing [KR,AR] (Right [0]) "Lag" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Exponential lag
lag2 :: UGen -> UGen -> UGen
lag2 in_ lagTime = mkUGen Nothing [KR,AR] (Right [0]) "Lag2" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Exponential lag
lag2UD :: UGen -> UGen -> UGen -> UGen
lag2UD in_ lagTimeU lagTimeD = mkUGen Nothing [KR,AR] (Right [0]) "Lag2UD" [in_,lagTimeU,lagTimeD] Nothing 1 (Special 0) NoId

-- | Exponential lag
lag3 :: UGen -> UGen -> UGen
lag3 in_ lagTime = mkUGen Nothing [KR,AR] (Right [0]) "Lag3" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Exponential lag
lag3UD :: UGen -> UGen -> UGen -> UGen
lag3UD in_ lagTimeU lagTimeD = mkUGen Nothing [KR,AR] (Right [0]) "Lag3UD" [in_,lagTimeU,lagTimeD] Nothing 1 (Special 0) NoId

-- | Lagged control input
lagControl :: Rate -> UGen -> UGen -> UGen
lagControl rate values lags = mkUGen Nothing [IR,KR] (Left rate) "LagControl" [values,lags] Nothing 1 (Special 0) NoId

-- | Read a control signal from a bus with a lag
lagIn :: Int -> Rate -> UGen -> UGen -> UGen
lagIn numChannels rate bus lag_ = mkUGen Nothing [KR] (Left rate) "LagIn" [bus,lag_] Nothing numChannels (Special 0) NoId

-- | Exponential lag
lagUD :: UGen -> UGen -> UGen -> UGen
lagUD in_ lagTimeU lagTimeD = mkUGen Nothing [KR,AR] (Right [0]) "LagUD" [in_,lagTimeU,lagTimeD] Nothing 1 (Special 0) NoId

-- | Output the last value before the input changed
lastValue :: UGen -> UGen -> UGen
lastValue in_ diff = mkUGen Nothing [KR,AR] (Right [0]) "LastValue" [in_,diff] Nothing 1 (Special 0) NoId

-- | Sample and hold
latch :: UGen -> UGen -> UGen
latch in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "Latch" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Latoocarfian chaotic generator
latoocarfianC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianC rate freq a b c d xi yi = mkUGen Nothing [AR] (Left rate) "LatoocarfianC" [freq,a,b,c,d,xi,yi] Nothing 1 (Special 0) NoId

-- | Latoocarfian chaotic generator
latoocarfianL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianL rate freq a b c d xi yi = mkUGen Nothing [AR] (Left rate) "LatoocarfianL" [freq,a,b,c,d,xi,yi] Nothing 1 (Special 0) NoId

-- | Latoocarfian chaotic generator
latoocarfianN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianN rate freq a b c d xi yi = mkUGen Nothing [AR] (Left rate) "LatoocarfianN" [freq,a,b,c,d,xi,yi] Nothing 1 (Special 0) NoId

-- | Remove DC
leakDC :: UGen -> UGen -> UGen
leakDC in_ coef = mkUGen Nothing [KR,AR] (Right [0]) "LeakDC" [in_,coef] Nothing 1 (Special 0) NoId

-- | Output least changed
leastChange :: Rate -> UGen -> UGen -> UGen
leastChange rate a b = mkUGen Nothing [KR,AR] (Left rate) "LeastChange" [a,b] Nothing 1 (Special 0) NoId

-- | Peak limiter
limiter :: UGen -> UGen -> UGen -> UGen
limiter in_ level dur = mkUGen Nothing [AR] (Right [0]) "Limiter" [in_,level,dur] Nothing 1 (Special 0) NoId

-- | Linear congruential chaotic generator
linCongC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongC rate freq a c m xi = mkUGen Nothing [AR] (Left rate) "LinCongC" [freq,a,c,m,xi] Nothing 1 (Special 0) NoId

-- | Linear congruential chaotic generator
linCongL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongL rate freq a c m xi = mkUGen Nothing [AR] (Left rate) "LinCongL" [freq,a,c,m,xi] Nothing 1 (Special 0) NoId

-- | Linear congruential chaotic generator
linCongN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongN rate freq a c m xi = mkUGen Nothing [AR] (Left rate) "LinCongN" [freq,a,c,m,xi] Nothing 1 (Special 0) NoId

-- | Map a linear range to an exponential range
linExp :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linExp in_ srclo srchi dstlo dsthi = mkUGen Nothing [KR,AR] (Right [0]) "LinExp" [in_,srclo,srchi,dstlo,dsthi] Nothing 1 (Special 0) NoId

-- | Two channel linear pan.
linPan2 :: UGen -> UGen -> UGen -> UGen
linPan2 in_ pos level = mkUGen Nothing [KR,AR] (Right [0]) "LinPan2" [in_,pos,level] Nothing 2 (Special 0) NoId

-- | Skewed random number generator.
linRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
linRand z lo hi minmax = mkUGen Nothing [IR] (Left IR) "LinRand" [lo,hi,minmax] Nothing 1 (Special 0) (toUId z)

-- | Two channel linear crossfade.
linXFade2 :: UGen -> UGen -> UGen -> UGen -> UGen
linXFade2 inA inB pan level = mkUGen Nothing [KR,AR] (Right [0,1]) "LinXFade2" [inA,inB,pan,level] Nothing 1 (Special 0) NoId

-- | Line generator.
line :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
line rate start end dur doneAction = mkUGen Nothing [KR,AR] (Left rate) "Line" [start,end,dur,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Simple linear envelope generator.
linen :: UGen -> UGen -> UGen -> UGen -> DoneAction -> UGen
linen gate_ attackTime susLevel releaseTime doneAction = mkUGen Nothing [KR] (Left KR) "Linen" [gate_,attackTime,susLevel,releaseTime,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Allocate a buffer local to the synth
localBuf :: ID a => a -> UGen -> UGen -> UGen
localBuf z numChannels numFrames = mkUGen Nothing [IR] (Left IR) "LocalBuf" [numChannels,numFrames] Nothing 1 (Special 0) (toUId z)

-- | Define and read from buses local to a synth.
localIn :: Int -> Rate -> UGen -> UGen
localIn numChannels rate default_ = mkUGen Nothing [KR,AR] (Left rate) "LocalIn" [] (Just default_) numChannels (Special 0) NoId

-- | Write to buses local to a synth.
localOut :: UGen -> UGen
localOut input = mkUGen Nothing [KR,AR] (Right [0]) "LocalOut" [] (Just input) 1 (Special 0) NoId

-- | Chaotic noise function
logistic :: Rate -> UGen -> UGen -> UGen -> UGen
logistic rate chaosParam freq init_ = mkUGen Nothing [KR,AR] (Left rate) "Logistic" [chaosParam,freq,init_] Nothing 1 (Special 0) NoId

-- | Lorenz chaotic generator
lorenzL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenzL rate freq s r b h xi yi zi = mkUGen Nothing [AR] (Left rate) "LorenzL" [freq,s,r,b,h,xi,yi,zi] Nothing 1 (Special 0) NoId

-- | Extraction of instantaneous loudness in sones
loudness :: Rate -> UGen -> UGen -> UGen -> UGen
loudness rate chain smask tmask = mkUGen Nothing [KR] (Left rate) "Loudness" [chain,smask,tmask] Nothing 1 (Special 0) NoId

-- | Mel frequency cepstral coefficients
mFCC :: Rate -> UGen -> UGen -> UGen
mFCC rate chain numcoeff = mkUGen Nothing [KR] (Left rate) "MFCC" [chain,numcoeff] Nothing 13 (Special 0) NoId

-- | Reduce precision.
mantissaMask :: UGen -> UGen -> UGen
mantissaMask in_ bits = mkUGen Nothing [KR,AR] (Right [0]) "MantissaMask" [in_,bits] Nothing 1 (Special 0) NoId

-- | Set the maximum number of local buffers in a synth
maxLocalBufs :: Rate -> UGen
maxLocalBufs rate = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "MaxLocalBufs" [] Nothing 1 (Special 0) NoId

-- | Median filter.
median :: UGen -> UGen -> UGen
median length_ in_ = mkUGen Nothing [KR,AR] (Right [1]) "Median" [length_,in_] Nothing 1 (Special 0) NoId

-- | Parametric filter.
midEQ :: UGen -> UGen -> UGen -> UGen -> UGen
midEQ in_ freq rq db = mkUGen Nothing [KR,AR] (Right [0]) "MidEQ" [in_,freq,rq,db] Nothing 1 (Special 0) NoId

-- | Minimum difference of two values in modulo arithmetics
modDif :: Rate -> UGen -> UGen -> UGen -> UGen
modDif rate x y mod_ = mkUGen Nothing [IR,KR,AR] (Left rate) "ModDif" [x,y,mod_] Nothing 1 (Special 0) NoId

-- | Moog VCF implementation, designed by Federico Fontana
moogFF :: UGen -> UGen -> UGen -> UGen -> UGen
moogFF in_ freq gain reset = mkUGen Nothing [KR,AR] (Right [0]) "MoogFF" [in_,freq,gain,reset] Nothing 1 (Special 0) NoId

-- | Output most changed.
mostChange :: UGen -> UGen -> UGen
mostChange a b = mkUGen Nothing [KR,AR] (Right [0,1]) "MostChange" [a,b] Nothing 1 (Special 0) NoId

-- | Mouse button UGen.
mouseButton :: Rate -> UGen -> UGen -> UGen -> UGen
mouseButton rate minval maxval lag_ = mkUGen Nothing [KR] (Left rate) "MouseButton" [minval,maxval,lag_] Nothing 1 (Special 0) NoId

-- | Cursor tracking UGen.
mouseX :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseX rate minval maxval warp lag_ = mkUGen Nothing [KR] (Left rate) "MouseX" [minval,maxval,(from_warp warp),lag_] Nothing 1 (Special 0) NoId

-- | Cursor tracking UGen.
mouseY :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseY rate minval maxval warp lag_ = mkUGen Nothing [KR] (Left rate) "MouseY" [minval,maxval,(from_warp warp),lag_] Nothing 1 (Special 0) NoId

-- | Sum of uniform distributions.
nRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
nRand z lo hi n = mkUGen Nothing [IR] (Left IR) "NRand" [lo,hi,n] Nothing 1 (Special 0) (toUId z)

-- | Flattens dynamics.
normalizer :: UGen -> UGen -> UGen -> UGen
normalizer in_ level dur = mkUGen Nothing [AR] (Right [0]) "Normalizer" [in_,level,dur] Nothing 1 (Special 0) NoId

-- | Number of audio busses.
numAudioBuses :: UGen
numAudioBuses = mkUGen Nothing [IR] (Left IR) "NumAudioBuses" [] Nothing 1 (Special 0) NoId

-- | Number of open buffers.
numBuffers :: UGen
numBuffers = mkUGen Nothing [IR] (Left IR) "NumBuffers" [] Nothing 1 (Special 0) NoId

-- | Number of control busses.
numControlBuses :: UGen
numControlBuses = mkUGen Nothing [IR] (Left IR) "NumControlBuses" [] Nothing 1 (Special 0) NoId

-- | Number of input busses.
numInputBuses :: UGen
numInputBuses = mkUGen Nothing [IR] (Left IR) "NumInputBuses" [] Nothing 1 (Special 0) NoId

-- | Number of output busses.
numOutputBuses :: UGen
numOutputBuses = mkUGen Nothing [IR] (Left IR) "NumOutputBuses" [] Nothing 1 (Special 0) NoId

-- | Number of currently running synths.
numRunningSynths :: UGen
numRunningSynths = mkUGen Nothing [IR,KR] (Left IR) "NumRunningSynths" [] Nothing 1 (Special 0) NoId

-- | Write a signal to a bus with sample accurate timing.
offsetOut :: UGen -> UGen -> UGen
offsetOut bus input = mkUGen Nothing [KR,AR] (Right [1]) "OffsetOut" [bus] (Just input) 1 (Special 0) NoId

-- | One pole filter.
onePole :: UGen -> UGen -> UGen
onePole in_ coef = mkUGen Nothing [KR,AR] (Right [0]) "OnePole" [in_,coef] Nothing 1 (Special 0) NoId

-- | One zero filter.
oneZero :: UGen -> UGen -> UGen
oneZero in_ coef = mkUGen Nothing [KR,AR] (Right [0]) "OneZero" [in_,coef] Nothing 1 (Special 0) NoId

-- | Onset detector
onsets :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
onsets chain threshold odftype relaxtime floor_ mingap medianspan whtype rawodf = mkUGen Nothing [KR] (Left KR) "Onsets" [chain,threshold,odftype,relaxtime,floor_,mingap,medianspan,whtype,rawodf] Nothing 1 (Special 0) NoId

-- | Interpolating wavetable oscillator.
osc :: Rate -> UGen -> UGen -> UGen -> UGen
osc rate bufnum freq phase = mkUGen Nothing [KR,AR] (Left rate) "Osc" [bufnum,freq,phase] Nothing 1 (Special 0) NoId

-- | Noninterpolating wavetable oscillator.
oscN :: Rate -> UGen -> UGen -> UGen -> UGen
oscN rate bufnum freq phase = mkUGen Nothing [KR,AR] (Left rate) "OscN" [bufnum,freq,phase] Nothing 1 (Special 0) NoId

-- | Write a signal to a bus.
out :: UGen -> UGen -> UGen
out bus input = mkUGen Nothing [KR,AR] (Right [1]) "Out" [bus] (Just input) 1 (Special 0) NoId

-- | Very fast sine grain with a parabolic envelope
pSinGrain :: Rate -> UGen -> UGen -> UGen -> UGen
pSinGrain rate freq dur amp = mkUGen Nothing [AR] (Left rate) "PSinGrain" [freq,dur,amp] Nothing 1 (Special 0) NoId

-- | Complex addition.
pv_Add :: UGen -> UGen -> UGen
pv_Add bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Add" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Scramble bins.
pv_BinScramble :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble z buffer wipe width trig_ = mkUGen Nothing [KR] (Left KR) "PV_BinScramble" [buffer,wipe,width,trig_] Nothing 1 (Special 0) (toUId z)

-- | Shift and stretch bin position.
pv_BinShift :: UGen -> UGen -> UGen -> UGen -> UGen
pv_BinShift buffer stretch shift interp = mkUGen Nothing [KR] (Left KR) "PV_BinShift" [buffer,stretch,shift,interp] Nothing 1 (Special 0) NoId

-- | Combine low and high bins from two inputs.
pv_BinWipe :: UGen -> UGen -> UGen -> UGen
pv_BinWipe bufferA bufferB wipe = mkUGen Nothing [KR] (Left KR) "PV_BinWipe" [bufferA,bufferB,wipe] Nothing 1 (Special 0) NoId

-- | Zero bins.
pv_BrickWall :: UGen -> UGen -> UGen
pv_BrickWall buffer wipe = mkUGen Nothing [KR] (Left KR) "PV_BrickWall" [buffer,wipe] Nothing 1 (Special 0) NoId

-- | Base class for UGens that alter FFT chains
pv_ChainUGen :: UGen -> UGen
pv_ChainUGen maxSize = mkUGen Nothing [KR] (Left KR) "PV_ChainUGen" [maxSize] Nothing 1 (Special 0) NoId

-- | Complex plane attack.
pv_ConformalMap :: UGen -> UGen -> UGen -> UGen
pv_ConformalMap buffer areal aimag = mkUGen Nothing [KR] (Left KR) "PV_ConformalMap" [buffer,areal,aimag] Nothing 1 (Special 0) NoId

-- | Complex conjugate
pv_Conj :: UGen -> UGen
pv_Conj buffer = mkUGen Nothing [KR] (Left KR) "PV_Conj" [buffer] Nothing 1 (Special 0) NoId

-- | Copy an FFT buffer
pv_Copy :: UGen -> UGen -> UGen
pv_Copy bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Copy" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Copy magnitudes and phases.
pv_CopyPhase :: UGen -> UGen -> UGen
pv_CopyPhase bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_CopyPhase" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Random phase shifting.
pv_Diffuser :: UGen -> UGen -> UGen
pv_Diffuser buffer trig_ = mkUGen Nothing [KR] (Left KR) "PV_Diffuser" [buffer,trig_] Nothing 1 (Special 0) NoId

-- | Complex division
pv_Div :: UGen -> UGen -> UGen
pv_Div bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Div" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | FFT onset detector.
pv_HainsworthFoote :: UGen -> UGen
pv_HainsworthFoote maxSize = mkUGen Nothing [KR,AR] (Left KR) "PV_HainsworthFoote" [maxSize] Nothing 1 (Special 0) NoId

-- | FFT feature detector for onset detection.
pv_JensenAndersen :: UGen -> UGen
pv_JensenAndersen maxSize = mkUGen Nothing [KR,AR] (Left KR) "PV_JensenAndersen" [maxSize] Nothing 1 (Special 0) NoId

-- | Pass bins which are a local maximum.
pv_LocalMax :: UGen -> UGen -> UGen
pv_LocalMax buffer threshold = mkUGen Nothing [KR] (Left KR) "PV_LocalMax" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Pass bins above a threshold.
pv_MagAbove :: UGen -> UGen -> UGen
pv_MagAbove buffer threshold = mkUGen Nothing [KR] (Left KR) "PV_MagAbove" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Pass bins below a threshold.
pv_MagBelow :: UGen -> UGen -> UGen
pv_MagBelow buffer threshold = mkUGen Nothing [KR] (Left KR) "PV_MagBelow" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Clip bins to a threshold.
pv_MagClip :: UGen -> UGen -> UGen
pv_MagClip buffer threshold = mkUGen Nothing [KR] (Left KR) "PV_MagClip" [buffer,threshold] Nothing 1 (Special 0) NoId

-- | Division of magnitudes
pv_MagDiv :: UGen -> UGen -> UGen -> UGen
pv_MagDiv bufferA bufferB zeroed = mkUGen Nothing [KR] (Left KR) "PV_MagDiv" [bufferA,bufferB,zeroed] Nothing 1 (Special 0) NoId

-- | Freeze magnitudes.
pv_MagFreeze :: UGen -> UGen -> UGen
pv_MagFreeze buffer freeze = mkUGen Nothing [KR] (Left KR) "PV_MagFreeze" [buffer,freeze] Nothing 1 (Special 0) NoId

-- | Multiply magnitudes.
pv_MagMul :: UGen -> UGen -> UGen
pv_MagMul bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_MagMul" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Multiply magnitudes by noise.
pv_MagNoise :: UGen -> UGen
pv_MagNoise buffer = mkUGen Nothing [KR] (Left KR) "PV_MagNoise" [buffer] Nothing 1 (Special 0) NoId

-- | shift and stretch magnitude bin position.
pv_MagShift :: UGen -> UGen -> UGen -> UGen
pv_MagShift buffer stretch shift = mkUGen Nothing [KR] (Left KR) "PV_MagShift" [buffer,stretch,shift] Nothing 1 (Special 0) NoId

-- | Average magnitudes across bins.
pv_MagSmear :: UGen -> UGen -> UGen
pv_MagSmear buffer bins = mkUGen Nothing [KR] (Left KR) "PV_MagSmear" [buffer,bins] Nothing 1 (Special 0) NoId

-- | Square magnitudes.
pv_MagSquared :: UGen -> UGen
pv_MagSquared buffer = mkUGen Nothing [KR] (Left KR) "PV_MagSquared" [buffer] Nothing 1 (Special 0) NoId

-- | Maximum magnitude.
pv_Max :: UGen -> UGen -> UGen
pv_Max bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Max" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Minimum magnitude.
pv_Min :: UGen -> UGen -> UGen
pv_Min bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Min" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Complex multiply.
pv_Mul :: UGen -> UGen -> UGen
pv_Mul bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Mul" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Shift phase.
pv_PhaseShift :: UGen -> UGen -> UGen -> UGen
pv_PhaseShift buffer shift integrate = mkUGen Nothing [KR] (Left KR) "PV_PhaseShift" [buffer,shift,integrate] Nothing 1 (Special 0) NoId

-- | Shift phase by 270 degrees.
pv_PhaseShift270 :: UGen -> UGen
pv_PhaseShift270 buffer = mkUGen Nothing [KR] (Left KR) "PV_PhaseShift270" [buffer] Nothing 1 (Special 0) NoId

-- | Shift phase by 90 degrees.
pv_PhaseShift90 :: UGen -> UGen
pv_PhaseShift90 buffer = mkUGen Nothing [KR] (Left KR) "PV_PhaseShift90" [buffer] Nothing 1 (Special 0) NoId

-- | Pass random bins.
pv_RandComb :: ID a => a -> UGen -> UGen -> UGen -> UGen
pv_RandComb z buffer wipe trig_ = mkUGen Nothing [KR] (Left KR) "PV_RandComb" [buffer,wipe,trig_] Nothing 1 (Special 0) (toUId z)

-- | Crossfade in random bin order.
pv_RandWipe :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe z bufferA bufferB wipe trig_ = mkUGen Nothing [KR] (Left KR) "PV_RandWipe" [bufferA,bufferB,wipe,trig_] Nothing 1 (Special 0) (toUId z)

-- | Make gaps in spectrum.
pv_RectComb :: UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb buffer numTeeth phase width = mkUGen Nothing [KR] (Left KR) "PV_RectComb" [buffer,numTeeth,phase,width] Nothing 1 (Special 0) NoId

-- | Make gaps in spectrum.
pv_RectComb2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb2 bufferA bufferB numTeeth phase width = mkUGen Nothing [KR] (Left KR) "PV_RectComb2" [bufferA,bufferB,numTeeth,phase,width] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
pv_Split :: UGen -> UGen -> UGen
pv_Split bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_Split" [bufferA,bufferB] Nothing 2 (Special 0) NoId

-- | Two channel equal power pan.
pan2 :: UGen -> UGen -> UGen -> UGen
pan2 in_ pos level = mkUGen Nothing [KR,AR] (Right [0]) "Pan2" [in_,pos,level] Nothing 2 (Special 0) NoId

-- | Four channel equal power pan.
pan4 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
pan4 rate in_ xpos ypos level = mkUGen Nothing [KR,AR] (Left rate) "Pan4" [in_,xpos,ypos,level] Nothing 4 (Special 0) NoId

-- | Azimuth panner
panAz :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
panAz numChannels in_ pos level width orientation = mkUGen Nothing [KR,AR] (Right [0]) "PanAz" [in_,pos,level,width,orientation] Nothing numChannels (Special 0) NoId

-- | Ambisonic B-format panner.
panB :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
panB rate in_ azimuth elevation gain = mkUGen Nothing [KR,AR] (Left rate) "PanB" [in_,azimuth,elevation,gain] Nothing 4 (Special 0) NoId

-- | 2D Ambisonic B-format panner.
panB2 :: Rate -> UGen -> UGen -> UGen -> UGen
panB2 rate in_ azimuth gain = mkUGen Nothing [KR,AR] (Left rate) "PanB2" [in_,azimuth,gain] Nothing 3 (Special 0) NoId

-- | Real-time partitioned convolution
partConv :: UGen -> UGen -> UGen -> UGen
partConv in_ fftsize irbufnum = mkUGen Nothing [AR] (Left AR) "PartConv" [in_,fftsize,irbufnum] Nothing 1 (Special 0) NoId

-- | When triggered, pauses a node.
pause :: Rate -> UGen -> UGen -> UGen
pause rate gate_ id_ = mkUGen Nothing [KR] (Left rate) "Pause" [gate_,id_] Nothing 1 (Special 0) NoId

-- | When triggered, pause enclosing synth.
pauseSelf :: Rate -> UGen -> UGen
pauseSelf rate in_ = mkUGen Nothing [KR] (Left rate) "PauseSelf" [in_] Nothing 1 (Special 0) NoId

-- | FIXME: PauseSelfWhenDone purpose.
pauseSelfWhenDone :: Rate -> UGen -> UGen
pauseSelfWhenDone rate src = mkUGen Nothing [KR] (Left rate) "PauseSelfWhenDone" [src] Nothing 1 (Special 0) NoId

-- | Track peak signal amplitude.
peak :: UGen -> UGen -> UGen
peak in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "Peak" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Track peak signal amplitude.
peakFollower :: UGen -> UGen -> UGen
peakFollower in_ decay_ = mkUGen Nothing [KR,AR] (Right [0]) "PeakFollower" [in_,decay_] Nothing 1 (Special 0) NoId

-- | A resettable linear ramp between two levels.
phasor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
phasor rate trig_ rate_ start end resetPos = mkUGen Nothing [KR,AR] (Left rate) "Phasor" [trig_,rate_,start,end,resetPos] Nothing 1 (Special 0) NoId

-- | Pink Noise.
pinkNoise :: ID a => a -> Rate -> UGen
pinkNoise z rate = mkUGen Nothing [KR,AR] (Left rate) "PinkNoise" [] Nothing 1 (Special 0) (toUId z)

-- | Autocorrelation pitch follower
pitch :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pitch in_ initFreq minFreq maxFreq execFreq maxBinsPerOctave median_ ampThreshold peakThreshold downSample clar = mkUGen Nothing [KR] (Left KR) "Pitch" [in_,initFreq,minFreq,maxFreq,execFreq,maxBinsPerOctave,median_,ampThreshold,peakThreshold,downSample,clar] Nothing 2 (Special 0) NoId

-- | Time domain pitch shifter.
pitchShift :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pitchShift in_ windowSize pitchRatio pitchDispersion timeDispersion = mkUGen Nothing [AR] (Right [0]) "PitchShift" [in_,windowSize,pitchRatio,pitchDispersion,timeDispersion] Nothing 1 (Special 0) NoId

-- | Sample playback oscillator.
playBuf :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> Loop -> DoneAction -> UGen
playBuf numChannels rate bufnum rate_ trigger startPos loop doneAction = mkUGen Nothing [KR,AR] (Left rate) "PlayBuf" [bufnum,rate_,trigger,startPos,(from_loop loop),(from_done_action doneAction)] Nothing numChannels (Special 0) NoId

-- | A Karplus-Strong UGen
pluck :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pluck in_ trig_ maxdelaytime delaytime decaytime coef = mkUGen Nothing [AR] (Right [0]) "Pluck" [in_,trig_,maxdelaytime,delaytime,decaytime,coef] Nothing 1 (Special 0) NoId

{-
-- | Print the current output value of a UGen
poll :: UGen -> UGen -> UGen -> UGen -> UGen
poll trig_ in_ label_ trigid = mkUGen Nothing [KR,AR] (Right [1]) "Poll" [trig_,in_,label_,trigid] Nothing 1 (Special 0) NoId
-}

-- | Band limited pulse wave.
pulse :: Rate -> UGen -> UGen -> UGen
pulse rate freq width = mkUGen Nothing [KR,AR] (Left rate) "Pulse" [freq,width] Nothing 1 (Special 0) NoId

-- | Pulse counter.
pulseCount :: UGen -> UGen -> UGen
pulseCount trig_ reset = mkUGen Nothing [KR,AR] (Right [0]) "PulseCount" [trig_,reset] Nothing 1 (Special 0) NoId

-- | Pulse divider.
pulseDivider :: UGen -> UGen -> UGen -> UGen
pulseDivider trig_ div_ start = mkUGen Nothing [KR,AR] (Right [0]) "PulseDivider" [trig_,div_,start] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
pureMultiOutUGen :: Rate -> UGen -> UGen
pureMultiOutUGen rate maxSize = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "PureMultiOutUGen" [maxSize] Nothing 1 (Special 0) NoId

-- | Pure UGen
pureUGen :: Rate -> UGen -> UGen
pureUGen rate maxSize = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "PureUGen" [maxSize] Nothing 1 (Special 0) NoId

-- | General quadratic map chaotic generator
quadC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadC rate freq a b c xi = mkUGen Nothing [AR] (Left rate) "QuadC" [freq,a,b,c,xi] Nothing 1 (Special 0) NoId

-- | General quadratic map chaotic generator
quadL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadL rate freq a b c xi = mkUGen Nothing [AR] (Left rate) "QuadL" [freq,a,b,c,xi] Nothing 1 (Special 0) NoId

-- | General quadratic map chaotic generator
quadN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadN rate freq a b c xi = mkUGen Nothing [AR] (Left rate) "QuadN" [freq,a,b,c,xi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rDelayMap :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
rDelayMap rate bufnum in_ dynamic spec = mkUGen Nothing [KR,AR] (Left rate) "RDelayMap" [bufnum,in_,dynamic,spec] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rDelaySet :: Rate -> UGen -> UGen -> UGen
rDelaySet rate in_ spec = mkUGen Nothing [KR,AR] (Left rate) "RDelaySet" [in_,spec] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rDelaySetB :: Rate -> UGen -> UGen -> UGen -> UGen
rDelaySetB rate bufnum in_ spec = mkUGen Nothing [KR,AR] (Left rate) "RDelaySetB" [bufnum,in_,spec] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rFreezer :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rFreezer rate bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops = mkUGen Nothing [KR,AR] (Left rate) "RFreezer" [bufnum,left,right,gain,increment,incrementOffset,incrementRandom,rightRandom,syncPhaseTrigger,randomizePhaseTrigger,numberOfLoops] Nothing 1 (Special 0) NoId

-- | A resonant high pass filter.
rhpf :: UGen -> UGen -> UGen -> UGen
rhpf in_ freq rq = mkUGen Nothing [KR,AR] (Right [0]) "RHPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | A resonant low pass filter.
rlpf :: UGen -> UGen -> UGen -> UGen
rlpf in_ freq rq = mkUGen Nothing [KR,AR] (Right [0]) "RLPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rLoopSet :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rLoopSet rate bufnum left right gain increment spec = mkUGen Nothing [KR,AR] (Left rate) "RLoopSet" [bufnum,left,right,gain,increment,spec] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rPlayTrace :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
rPlayTrace rate bufnum degree rate_ axis = mkUGen Nothing [KR,AR] (Left rate) "RPlayTrace" [bufnum,degree,rate_,axis] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rShufflerB :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rShufflerB bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta = mkUGen Nothing [KR,AR] (Left AR) "RShufflerB" [bufnum,readLocationMinima,readLocationMaxima,readIncrementMinima,readIncrementMaxima,durationMinima,durationMaxima,envelopeAmplitudeMinima,envelopeAmplitudeMaxima,envelopeShapeMinima,envelopeShapeMaxima,envelopeSkewMinima,envelopeSkewMaxima,stereoLocationMinima,stereoLocationMaxima,interOffsetTimeMinima,interOffsetTimeMaxima,ftableReadLocationIncrement,readIncrementQuanta,interOffsetTimeQuanta] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
rShufflerL :: Rate -> UGen -> UGen -> UGen -> UGen
rShufflerL rate in_ fragmentSize maxDelay = mkUGen Nothing [KR,AR] (Left rate) "RShufflerL" [in_,fragmentSize,maxDelay] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rTraceRd :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
rTraceRd rate bufnum degree index_ axis = mkUGen Nothing [KR,AR] (Left rate) "RTraceRd" [bufnum,degree,index_,axis] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rTraceRdX :: Rate -> UGen -> UGen -> UGen -> UGen
rTraceRdX rate bufnum degree index_ = mkUGen Nothing [KR] (Left rate) "RTraceRdX" [bufnum,degree,index_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rTraceRdY :: Rate -> UGen -> UGen -> UGen -> UGen
rTraceRdY rate bufnum degree index_ = mkUGen Nothing [KR] (Left rate) "RTraceRdY" [bufnum,degree,index_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
rTraceRdZ :: Rate -> UGen -> UGen -> UGen -> UGen
rTraceRdZ rate bufnum degree index_ = mkUGen Nothing [KR] (Left rate) "RTraceRdZ" [bufnum,degree,index_] Nothing 1 (Special 0) NoId

-- | Number of radians per sample.
radiansPerSample :: UGen
radiansPerSample = mkUGen Nothing [IR] (Left IR) "RadiansPerSample" [] Nothing 1 (Special 0) NoId

-- | Break a continuous signal into line segments
ramp :: UGen -> UGen -> UGen
ramp in_ lagTime = mkUGen Nothing [KR,AR] (Right [0]) "Ramp" [in_,lagTime] Nothing 1 (Special 0) NoId

-- | Single random number generator.
rand :: ID a => a -> UGen -> UGen -> UGen
rand z lo hi = mkUGen Nothing [IR] (Left IR) "Rand" [lo,hi] Nothing 1 (Special 0) (toUId z)

-- | Set the synth's random generator ID.
randID :: Rate -> UGen -> UGen
randID rate id_ = mkUGen Nothing [IR,KR] (Left rate) "RandID" [id_] Nothing 1 (Special 0) NoId

-- | Sets the synth's random generator seed.
randSeed :: Rate -> UGen -> UGen -> UGen
randSeed rate trig_ seed = mkUGen Nothing [IR,KR,AR] (Left rate) "RandSeed" [trig_,seed] Nothing 1 (Special 0) NoId

-- | Record or overdub into a Buffer.
recordBuf :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> Loop -> UGen -> DoneAction -> UGen -> UGen
recordBuf rate bufnum offset recLevel preLevel run loop trigger doneAction inputArray = mkUGen Nothing [KR,AR] (Left rate) "RecordBuf" [bufnum,offset,recLevel,preLevel,run,(from_loop loop),trigger,(from_done_action doneAction)] (Just inputArray) 1 (Special 0) NoId

-- | Send signal to a bus, overwriting previous contents.
replaceOut :: UGen -> UGen -> UGen
replaceOut bus input = mkUGen Nothing [KR,AR] (Right [1]) "ReplaceOut" [bus] (Just input) 1 (Special 0) NoId

-- | Resonant filter.
resonz :: UGen -> UGen -> UGen -> UGen
resonz in_ freq bwr = mkUGen Nothing [KR,AR] (Right [0]) "Resonz" [in_,freq,bwr] Nothing 1 (Special 0) NoId

-- | Ringing filter.
ringz :: UGen -> UGen -> UGen -> UGen
ringz in_ freq decaytime = mkUGen Nothing [KR,AR] (Right [0]) "Ringz" [in_,freq,decaytime] Nothing 1 (Special 0) NoId

-- | Rotate a sound field.
rotate2 :: UGen -> UGen -> UGen -> UGen
rotate2 x y pos = mkUGen Nothing [KR,AR] (Right [0,1]) "Rotate2" [x,y,pos] Nothing 2 (Special 0) NoId

-- | Track maximum level.
runningMax :: UGen -> UGen -> UGen
runningMax in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "RunningMax" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Track minimum level.
runningMin :: UGen -> UGen -> UGen
runningMin in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "RunningMin" [in_,trig_] Nothing 1 (Special 0) NoId

-- | Running sum over n frames
runningSum :: UGen -> UGen -> UGen
runningSum in_ numsamp = mkUGen Nothing [KR,AR] (Right [0]) "RunningSum" [in_,numsamp] Nothing 1 (Special 0) NoId

-- | Second order filter section (biquad).
sos :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sos in_ a0 a1 a2 b1 b2 = mkUGen Nothing [KR,AR] (Right [0]) "SOS" [in_,a0,a1,a2,b1,b2] Nothing 1 (Special 0) NoId

-- | Duration of one sample.
sampleDur :: UGen
sampleDur = mkUGen Nothing [IR] (Left IR) "SampleDur" [] Nothing 1 (Special 0) NoId

-- | Server sample rate.
sampleRate :: UGen
sampleRate = mkUGen Nothing [IR] (Left IR) "SampleRate" [] Nothing 1 (Special 0) NoId

-- | Band limited sawtooth.
saw :: Rate -> UGen -> UGen
saw rate freq = mkUGen Nothing [KR,AR] (Left rate) "Saw" [freq] Nothing 1 (Special 0) NoId

-- | Schmidt trigger.
schmidt :: Rate -> UGen -> UGen -> UGen -> UGen
schmidt rate in_ lo hi = mkUGen Nothing [IR,KR,AR] (Left rate) "Schmidt" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | FIXME: ScopeOut purpose.
scopeOut :: Rate -> UGen -> UGen -> UGen
scopeOut rate inputArray bufnum = mkUGen Nothing [KR,AR] (Left rate) "ScopeOut" [inputArray,bufnum] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
scopeOut2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
scopeOut2 rate inputArray scopeNum maxFrames scopeFrames = mkUGen Nothing [KR,AR] (Left rate) "ScopeOut2" [inputArray,scopeNum,maxFrames,scopeFrames] Nothing 1 (Special 0) NoId

-- | Select output from an array of inputs.
select :: UGen -> UGen -> UGen
select which array = mkUGen Nothing [KR,AR] (Right [0,1]) "Select" [which] (Just array) 1 (Special 0) NoId

-- | Send a trigger message from the server back to the client.
sendTrig :: UGen -> UGen -> UGen -> UGen
sendTrig in_ id_ value = mkUGen Nothing [KR,AR] (Right [0]) "SendTrig" [in_,id_,value] Nothing 1 (Special 0) NoId

-- | Set-reset flip flop.
setResetFF :: UGen -> UGen -> UGen
setResetFF trig_ reset = mkUGen Nothing [KR,AR] (Right [0]) "SetResetFF" [trig_,reset] Nothing 1 (Special 0) NoId

-- | Wave shaper.
shaper :: UGen -> UGen -> UGen
shaper bufnum in_ = mkUGen Nothing [KR,AR] (Right [1]) "Shaper" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Interpolating sine wavetable oscillator.
sinOsc :: Rate -> UGen -> UGen -> UGen
sinOsc rate freq phase = mkUGen Nothing [KR,AR] (Left rate) "SinOsc" [freq,phase] Nothing 1 (Special 0) NoId

-- | Feedback FM oscillator
sinOscFB :: Rate -> UGen -> UGen -> UGen
sinOscFB rate freq feedback = mkUGen Nothing [KR,AR] (Left rate) "SinOscFB" [freq,feedback] Nothing 1 (Special 0) NoId

-- | Slew rate limiter.
slew :: UGen -> UGen -> UGen -> UGen
slew in_ up dn = mkUGen Nothing [KR,AR] (Right [0]) "Slew" [in_,up,dn] Nothing 1 (Special 0) NoId

-- | Slope of signal
slope :: UGen -> UGen
slope in_ = mkUGen Nothing [KR,AR] (Right [0]) "Slope" [in_] Nothing 1 (Special 0) NoId

-- | Spectral centroid
specCentroid :: Rate -> UGen -> UGen
specCentroid rate buffer = mkUGen Nothing [KR] (Left rate) "SpecCentroid" [buffer] Nothing 1 (Special 0) NoId

-- | Spectral Flatness measure
specFlatness :: Rate -> UGen -> UGen
specFlatness rate buffer = mkUGen Nothing [KR] (Left rate) "SpecFlatness" [buffer] Nothing 1 (Special 0) NoId

-- | Find a percentile of FFT magnitude spectrum
specPcile :: Rate -> UGen -> UGen -> UGen -> UGen
specPcile rate buffer fraction interpolate = mkUGen Nothing [KR] (Left rate) "SpecPcile" [buffer,fraction,interpolate] Nothing 1 (Special 0) NoId

-- | physical model of resonating spring
spring :: Rate -> UGen -> UGen -> UGen -> UGen
spring rate in_ spring_ damp = mkUGen Nothing [KR,AR] (Left rate) "Spring" [in_,spring_,damp] Nothing 1 (Special 0) NoId

-- | Standard map chaotic generator
standardL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
standardL rate freq k xi yi = mkUGen Nothing [AR] (Left rate) "StandardL" [freq,k,xi,yi] Nothing 1 (Special 0) NoId

-- | Standard map chaotic generator
standardN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
standardN rate freq k xi yi = mkUGen Nothing [AR] (Left rate) "StandardN" [freq,k,xi,yi] Nothing 1 (Special 0) NoId

-- | Pulse counter.
stepper :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stepper trig_ reset min_ max_ step resetval = mkUGen Nothing [KR,AR] (Right [0]) "Stepper" [trig_,reset,min_,max_,step,resetval] Nothing 1 (Special 0) NoId

-- | Stereo real-time convolver with linear interpolation
stereoConvolution2L :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stereoConvolution2L rate in_ kernelL kernelR trigger framesize crossfade = mkUGen Nothing [AR] (Left rate) "StereoConvolution2L" [in_,kernelL,kernelR,trigger,framesize,crossfade] Nothing 2 (Special 0) NoId

-- | Offset from synth start within one sample.
subsampleOffset :: UGen
subsampleOffset = mkUGen Nothing [IR] (Left IR) "SubsampleOffset" [] Nothing 1 (Special 0) NoId

-- | Sum three signals
sum3 :: UGen -> UGen -> UGen -> UGen
sum3 in0 in1 in2 = mkUGen Nothing [IR,KR,AR,DR] (Right [0,1,2]) "Sum3" [in0,in1,in2] Nothing 1 (Special 0) NoId

-- | Sum four signals
sum4 :: UGen -> UGen -> UGen -> UGen -> UGen
sum4 in0 in1 in2 in3 = mkUGen Nothing [IR,KR,AR,DR] (Right [0,1,2,3]) "Sum4" [in0,in1,in2,in3] Nothing 1 (Special 0) NoId

-- | Triggered linear ramp
sweep :: UGen -> UGen -> UGen
sweep trig_ rate_ = mkUGen Nothing [KR,AR] (Right [0]) "Sweep" [trig_,rate_] Nothing 1 (Special 0) NoId

-- | Hard sync sawtooth wave.
syncSaw :: Rate -> UGen -> UGen -> UGen
syncSaw rate syncFreq sawFreq = mkUGen Nothing [KR,AR] (Left rate) "SyncSaw" [syncFreq,sawFreq] Nothing 1 (Special 0) NoId

-- | Control rate trigger to audio rate trigger converter
t2A :: UGen -> UGen -> UGen
t2A in_ offset = mkUGen Nothing [AR] (Left AR) "T2A" [in_,offset] Nothing 1 (Special 0) NoId

-- | Audio rate trigger to control rate trigger converter
t2K :: Rate -> UGen -> UGen
t2K rate in_ = mkUGen Nothing [KR] (Left rate) "T2K" [in_] Nothing 1 (Special 0) NoId

-- | physical model of bouncing object
tBall :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
tBall rate in_ g damp friction = mkUGen Nothing [KR,AR] (Left rate) "TBall" [in_,g,damp,friction] Nothing 1 (Special 0) NoId

-- | Trigger delay.
tDelay :: UGen -> UGen -> UGen
tDelay in_ dur = mkUGen Nothing [KR,AR] (Right [0]) "TDelay" [in_,dur] Nothing 1 (Special 0) NoId

-- | Demand results as trigger from demand rate UGens.
tDuty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen -> UGen
tDuty rate dur reset doneAction level gapFirst = mkUGen Nothing [KR,AR] (Left rate) "TDuty" [dur,reset,(from_done_action doneAction),level,gapFirst] Nothing 1 (Special 0) NoId

-- | Triggered exponential random number generator.
tExpRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tExpRand z lo hi trig_ = mkUGen Nothing [KR,AR] (Right [2]) "TExpRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | Buffer granulator.
tGrains :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains numChannels trigger bufnum rate_ centerPos dur pan amp interp = mkUGen Nothing [AR] (Left AR) "TGrains" [trigger,bufnum,rate_,centerPos,dur,pan,amp,interp] Nothing numChannels (Special 0) NoId

-- | Triggered integer random number generator.
tIRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tIRand z lo hi trig_ = mkUGen Nothing [KR,AR] (Right [2]) "TIRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | Triggered random number generator.
tRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tRand z lo hi trig_ = mkUGen Nothing [KR,AR] (Right [2]) "TRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | Triggered windex.
tWindex :: ID a => a -> UGen -> UGen -> UGen -> UGen
tWindex z in_ normalize array = mkUGen Nothing [KR,AR] (Right [0]) "TWindex" [in_,normalize] (Just array) 1 (Special 0) (toUId z)

-- | Single tap into a delayline
tap :: Int -> Rate -> UGen -> UGen -> UGen
tap numChannels rate bufnum delaytime = mkUGen Nothing [AR] (Left rate) "Tap" [bufnum,delaytime] Nothing numChannels (Special 0) NoId

-- | Returns time since last triggered.
timer :: UGen -> UGen
timer trig_ = mkUGen Nothing [KR,AR] (Right [0]) "Timer" [trig_] Nothing 1 (Special 0) NoId

-- | Toggle flip flop.
toggleFF :: UGen -> UGen
toggleFF trig_ = mkUGen Nothing [KR,AR] (Right [0]) "ToggleFF" [trig_] Nothing 1 (Special 0) NoId

-- | Timed trigger.
trig :: UGen -> UGen -> UGen
trig in_ dur = mkUGen Nothing [KR,AR] (Right [0]) "Trig" [in_,dur] Nothing 1 (Special 0) NoId

-- | Timed trigger.
trig1 :: UGen -> UGen -> UGen
trig1 in_ dur = mkUGen Nothing [KR,AR] (Right [0]) "Trig1" [in_,dur] Nothing 1 (Special 0) NoId

-- | FIXME: TrigControl purpose.
trigControl :: Rate -> UGen -> UGen
trigControl rate values = mkUGen Nothing [IR,KR] (Left rate) "TrigControl" [values] Nothing 1 (Special 0) NoId

-- | Two pole filter.
twoPole :: UGen -> UGen -> UGen -> UGen
twoPole in_ freq radius = mkUGen Nothing [KR,AR] (Right [0]) "TwoPole" [in_,freq,radius] Nothing 1 (Special 0) NoId

-- | Two zero filter.
twoZero :: UGen -> UGen -> UGen -> UGen
twoZero in_ freq radius = mkUGen Nothing [KR,AR] (Right [0]) "TwoZero" [in_,freq,radius] Nothing 1 (Special 0) NoId

-- | Apply a unary operation to the values of an input ugen
unaryOpUGen :: UGen -> UGen
unaryOpUGen a = mkUGen Nothing [IR,KR,AR,DR] (Right [0]) "UnaryOpUGen" [a] Nothing 1 (Special 0) NoId

-- | Stream in audio from a file, with variable rate
vDiskIn :: Int -> UGen -> UGen -> Loop -> UGen -> UGen
vDiskIn numChannels bufnum rate_ loop sendID = mkUGen Nothing [AR] (Left AR) "VDiskIn" [bufnum,rate_,(from_loop loop),sendID] Nothing numChannels (Special 0) NoId

-- | Variable wavetable oscillator.
vOsc :: Rate -> UGen -> UGen -> UGen -> UGen
vOsc rate bufpos freq phase = mkUGen Nothing [KR,AR] (Left rate) "VOsc" [bufpos,freq,phase] Nothing 1 (Special 0) NoId

-- | Three variable wavetable oscillators.
vOsc3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vOsc3 rate bufpos freq1 freq2 freq3 = mkUGen Nothing [KR,AR] (Left rate) "VOsc3" [bufpos,freq1,freq2,freq3] Nothing 1 (Special 0) NoId

-- | Variable shaped lag
varLag :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
varLag rate in_ time curvature warp start = mkUGen Nothing [KR,AR] (Left rate) "VarLag" [in_,time,curvature,warp,start] Nothing 1 (Special 0) NoId

-- | Variable duty saw
varSaw :: Rate -> UGen -> UGen -> UGen -> UGen
varSaw rate freq iphase width = mkUGen Nothing [KR,AR] (Left rate) "VarSaw" [freq,iphase,width] Nothing 1 (Special 0) NoId

-- | The Vibrato oscillator models a slow frequency modulation.
vibrato :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
vibrato z rate freq rate_ depth delay onset rateVariation depthVariation iphase = mkUGen Nothing [KR,AR] (Left rate) "Vibrato" [freq,rate_,depth,delay,onset,rateVariation,depthVariation,iphase] Nothing 1 (Special 0) (toUId z)

-- | Warp a buffer with a time pointer
warp1 :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
warp1 numChannels bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp = mkUGen Nothing [AR] (Left AR) "Warp1" [bufnum,pointer,freqScale,windowSize,envbufnum,overlaps,windowRandRatio,interp] Nothing numChannels (Special 0) NoId

-- | White noise.
whiteNoise :: ID a => a -> Rate -> UGen
whiteNoise z rate = mkUGen Nothing [KR,AR] (Left rate) "WhiteNoise" [] Nothing 1 (Special 0) (toUId z)

-- | (Undocumented class)
widthFirstUGen :: Rate -> UGen -> UGen
widthFirstUGen rate maxSize = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "WidthFirstUGen" [maxSize] Nothing 1 (Special 0) NoId

-- | Wrap a signal outside given thresholds.
wrap :: UGen -> UGen -> UGen -> UGen
wrap in_ lo hi = mkUGen Nothing [IR,KR,AR] (Right [0]) "Wrap" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Index into a table with a signal.
wrapIndex :: UGen -> UGen -> UGen
wrapIndex bufnum in_ = mkUGen Nothing [KR,AR] (Right [1]) "WrapIndex" [bufnum,in_] Nothing 1 (Special 0) NoId

-- | Equal power two channel cross fade.
xFade2 :: UGen -> UGen -> UGen -> UGen -> UGen
xFade2 inA inB pan level = mkUGen Nothing [KR,AR] (Right [0,1]) "XFade2" [inA,inB,pan,level] Nothing 1 (Special 0) NoId

-- | Exponential line generator.
xLine :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
xLine rate start end dur doneAction = mkUGen Nothing [KR,AR] (Left rate) "XLine" [start,end,dur,(from_done_action doneAction)] Nothing 1 (Special 0) NoId

-- | Send signal to a bus, crossfading with previous contents.
xOut :: UGen -> UGen -> UGen -> UGen
xOut bus xfade input = mkUGen Nothing [KR,AR] (Right [2]) "XOut" [bus,xfade] (Just input) 1 (Special 0) NoId

-- | Zero crossing frequency follower
zeroCrossing :: UGen -> UGen
zeroCrossing in_ = mkUGen Nothing [KR,AR] (Right [0]) "ZeroCrossing" [in_] Nothing 1 (Special 0) NoId

-- | Multiply add
mulAdd :: UGen -> UGen -> UGen -> UGen
mulAdd in_ mul add = mkUGen Nothing [IR,KR,AR] (Right [0]) "MulAdd" [in_,mul,add] Nothing 1 (Special 0) NoId
