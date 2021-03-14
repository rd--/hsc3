-- | SC3 External UGen bindings (auto-generated).
module Sound.SC3.UGen.Bindings.DB.External where

--import Sound.SC3.Common.Enum
--import Sound.SC3.Common.Envelope
import Sound.SC3.Common.Rate
import Sound.SC3.Common.UId
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | (Undocumented class)
--
--  A2B [AR] a=0.0 b=0.0 c=0.0 d=0.0
a2b :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
a2b rate a b c d = mkUGen Nothing [AR] (Left rate) "A2B" [a,b,c,d] Nothing 4 (Special 0) NoId

-- | Emulator of the AY (aka YM) soundchip, used in Spectrum/Atari
--
--  AY [AR] tonea=1777.0 toneb=1666.0 tonec=1555.0 noise=1.0 control=7.0 vola=15.0 volb=15.0 volc=15.0 envfreq=4.0 envstyle=1.0 chiptype=0.0
ay :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
ay rate tonea toneb tonec noise control_ vola volb volc envfreq envstyle chiptype = mkUGen Nothing [AR] (Left rate) "AY" [tonea,toneb,tonec,noise,control_,vola,volb,volc,envfreq,envstyle,chiptype] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Allpass1 [AR] in=0.0 freq=1200.0
allpass1 :: Rate -> UGen -> UGen -> UGen
allpass1 rate in_ freq = mkUGen Nothing [AR] (Left rate) "Allpass1" [in_,freq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Allpass2 [AR] in=0.0 freq=1200.0 rq=1.0
allpass2 :: Rate -> UGen -> UGen -> UGen -> UGen
allpass2 rate in_ freq rq = mkUGen Nothing [AR] (Left rate) "Allpass2" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | amplitude follower (deprecated)
--
--  AmplitudeMod [KR,AR] in=0.0 attackTime=1.0e-2 releaseTime=1.0e-2
amplitudeMod :: Rate -> UGen -> UGen -> UGen -> UGen
amplitudeMod rate in_ attackTime releaseTime = mkUGen Nothing [KR,AR] (Left rate) "AmplitudeMod" [in_,attackTime,releaseTime] Nothing 1 (Special 0) NoId

-- | event analyser (BBCut)
--
--  AnalyseEvents2 [AR] in=0.0 bufnum=0.0 threshold=0.34 triggerid=101.0 circular=0.0 pitch=0.0
analyseEvents2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
analyseEvents2 rate in_ bufnum threshold triggerid circular pitch_ = mkUGen Nothing [AR] (Left rate) "AnalyseEvents2" [in_,bufnum,threshold,triggerid,circular,pitch_] Nothing 1 (Special 0) NoId

-- | 2-species Predator-Prey model
--
--  ArneodoCoulletTresser [AR] freq=22050.0 alpha=1.5 h=5.0e-2 xi=0.5 yi=0.5 zi=0.5
arneodoCoulletTresser :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
arneodoCoulletTresser rate freq alpha h xi yi zi = mkUGen Nothing [AR] (Left rate) "ArneodoCoulletTresser" [freq,alpha,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | detect the largest value (and its position) in an array of UGens
--
--  ArrayMax [KR,AR] *array=0.0;    MCE=1, FILTER: TRUE
arrayMax :: UGen -> UGen
arrayMax array = mkUGen Nothing [KR,AR] (Right [0]) "ArrayMax" [] (Just [array]) 2 (Special 0) NoId

-- | detect the smallest value (and its position) in an array of UGens
--
--  ArrayMin [KR,AR] *array=0.0;    MCE=1, FILTER: TRUE
arrayMin :: UGen -> UGen
arrayMin array = mkUGen Nothing [KR,AR] (Right [0]) "ArrayMin" [] (Just [array]) 2 (Special 0) NoId

-- | Use Amp data from a given partial
--
--  AtsAmp [KR,AR] atsbuffer=0.0 partialNum=0.0 filePointer=0.0
atsAmp :: Rate -> UGen -> UGen -> UGen -> UGen
atsAmp rate atsbuffer partialNum filePointer = mkUGen Nothing [KR,AR] (Left rate) "AtsAmp" [atsbuffer,partialNum,filePointer] Nothing 1 (Special 0) NoId

-- | (put short description here)
--
--  AtsBand [AR] atsbuffer=0.0 band=0.0 filePointer=0.0
atsBand :: Rate -> UGen -> UGen -> UGen -> UGen
atsBand rate atsbuffer band filePointer = mkUGen Nothing [AR] (Left rate) "AtsBand" [atsbuffer,band,filePointer] Nothing 1 (Special 0) NoId

-- | Use Freq data from a given partial
--
--  AtsFreq [KR,AR] atsbuffer=0.0 partialNum=0.0 filePointer=0.0
atsFreq :: Rate -> UGen -> UGen -> UGen -> UGen
atsFreq rate atsbuffer partialNum filePointer = mkUGen Nothing [KR,AR] (Left rate) "AtsFreq" [atsbuffer,partialNum,filePointer] Nothing 1 (Special 0) NoId

-- | Resynthesize sine and noise data from an ATS analysis file
--
--  AtsNoiSynth [AR] atsbuffer=0.0 numPartials=0.0 partialStart=0.0 partialSkip=1.0 filePointer=0.0 sinePct=1.0 noisePct=1.0 freqMul=1.0 freqAdd=0.0 numBands=25.0 bandStart=0.0 bandSkip=1.0
atsNoiSynth :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atsNoiSynth rate atsbuffer numPartials partialStart partialSkip filePointer sinePct noisePct freqMul freqAdd numBands bandStart bandSkip = mkUGen Nothing [AR] (Left rate) "AtsNoiSynth" [atsbuffer,numPartials,partialStart,partialSkip,filePointer,sinePct,noisePct,freqMul,freqAdd,numBands,bandStart,bandSkip] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AtsNoise [KR,AR] atsbuffer=0.0 bandNum=0.0 filePointer=0.0
atsNoise :: Rate -> UGen -> UGen -> UGen -> UGen
atsNoise rate atsbuffer bandNum filePointer = mkUGen Nothing [KR,AR] (Left rate) "AtsNoise" [atsbuffer,bandNum,filePointer] Nothing 1 (Special 0) NoId

-- | One UGen to return both Amp and Freq info
--
--  AtsParInfo [KR,AR] atsbuffer=0.0 partialNum=0.0 filePointer=0.0
atsParInfo :: Rate -> UGen -> UGen -> UGen -> UGen
atsParInfo rate atsbuffer partialNum filePointer = mkUGen Nothing [KR,AR] (Left rate) "AtsParInfo" [atsbuffer,partialNum,filePointer] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  AtsPartial [AR] atsbuffer=0.0 partial=0.0 filePointer=0.0 freqMul=1.0 freqAdd=0.0
atsPartial :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atsPartial rate atsbuffer partial filePointer freqMul freqAdd = mkUGen Nothing [AR] (Left rate) "AtsPartial" [atsbuffer,partial,filePointer,freqMul,freqAdd] Nothing 1 (Special 0) NoId

-- | Resynthesize sine data from an ATS analysis file
--
--  AtsSynth [AR] atsbuffer=0.0 numPartials=0.0 partialStart=0.0 partialSkip=1.0 filePointer=0.0 freqMul=1.0 freqAdd=0.0
atsSynth :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atsSynth rate atsbuffer numPartials partialStart partialSkip filePointer freqMul freqAdd = mkUGen Nothing [AR] (Left rate) "AtsSynth" [atsbuffer,numPartials,partialStart,partialSkip,filePointer,freqMul,freqAdd] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AtsUGen [] maxSize=0.0
atsUGen :: Rate -> UGen -> UGen
atsUGen rate maxSize = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "AtsUGen" [maxSize] Nothing 1 (Special 0) NoId

-- | Detect onsets and assess the nature of the attack slope
--
--  AttackSlope [KR] input=0.0 windowsize=1024.0 peakpicksize=20.0 leak=0.999 energythreshold=1.0e-2 sumthreshold=20.0 mingap=30.0 numslopesaveraged=10.0
attackSlope :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
attackSlope rate input windowsize peakpicksize leak energythreshold sumthreshold mingap numslopesaveraged = mkUGen Nothing [KR] (Left rate) "AttackSlope" [input,windowsize,peakpicksize,leak,energythreshold,sumthreshold,mingap,numslopesaveraged] Nothing 6 (Special 0) NoId

-- | (Undocumented class)
--
--  AudioMSG [AR] in=0.0 index=0.0;    FILTER: TRUE
audioMSG :: UGen -> UGen -> UGen
audioMSG in_ index_ = mkUGen Nothing [AR] (Right [0]) "AudioMSG" [in_,index_] Nothing 1 (Special 0) NoId

-- | calculates mean average of audio or control rate signal
--
--  AverageOutput [KR,AR] in=0.0 trig=0.0;    FILTER: TRUE
averageOutput :: UGen -> UGen -> UGen
averageOutput in_ trig_ = mkUGen Nothing [KR,AR] (Right [0]) "AverageOutput" [in_,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  B2A [AR] w=0.0 x=0.0 y=0.0 z=0.0
b2a :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
b2a rate w x y z = mkUGen Nothing [AR] (Left rate) "B2A" [w,x,y,z] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  B2Ster [AR] w=0.0 x=0.0 y=0.0
b2Ster :: Rate -> UGen -> UGen -> UGen -> UGen
b2Ster rate w x y = mkUGen Nothing [AR] (Left rate) "B2Ster" [w,x,y] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  B2UHJ [AR] w=0.0 x=0.0 y=0.0
b2uhj :: Rate -> UGen -> UGen -> UGen -> UGen
b2uhj rate w x y = mkUGen Nothing [AR] (Left rate) "B2UHJ" [w,x,y] Nothing 2 (Special 0) NoId

-- | MultiOut BetaBlocker VChip
--
--  BBlockerBuf [AR] freq=0.0 bufnum=0.0 startpoint=0.0
bBlockerBuf :: Rate -> UGen -> UGen -> UGen -> UGen
bBlockerBuf rate freq bufnum startpoint = mkUGen Nothing [AR] (Left rate) "BBlockerBuf" [freq,bufnum,startpoint] Nothing 9 (Special 0) NoId

-- | 3D Ambisonic decoder
--
--  BFDecode1 [AR] w=0.0 x=0.0 y=0.0 z=0.0 azimuth=0.0 elevation=0.0 wComp=0.0
bfDecode1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bfDecode1 rate w x y z azimuth elevation wComp = mkUGen Nothing [AR] (Left rate) "BFDecode1" [w,x,y,z,azimuth,elevation,wComp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BFDecoder [] maxSize=0.0
bfDecoder :: Rate -> UGen -> UGen
bfDecoder rate maxSize = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "BFDecoder" [maxSize] Nothing 1 (Special 0) NoId

-- | Ambisonic B format encoder
--
--  BFEncode1 [AR] in=0.0 azimuth=0.0 elevation=0.0 rho=1.0 gain=1.0 wComp=0.0
bfEncode1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bfEncode1 rate in_ azimuth elevation rho gain wComp = mkUGen Nothing [AR] (Left rate) "BFEncode1" [in_,azimuth,elevation,rho,gain,wComp] Nothing 4 (Special 0) NoId

-- | Ambisonic B format encoder
--
--  BFEncode2 [AR] in=0.0 point_x=1.0 point_y=1.0 elevation=0.0 gain=1.0 wComp=0.0
bfEncode2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bfEncode2 rate in_ point_x point_y elevation gain wComp = mkUGen Nothing [AR] (Left rate) "BFEncode2" [in_,point_x,point_y,elevation,gain,wComp] Nothing 4 (Special 0) NoId

-- | Ambisonic B format encoder for stereo signals
--
--  BFEncodeSter [AR] l=0.0 r=0.0 azimuth=0.0 width=1.5707963267949 elevation=0.0 rho=1.0 gain=1.0 wComp=0.0
bfEncodeSter :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bfEncodeSter rate l r azimuth width elevation rho gain wComp = mkUGen Nothing [AR] (Left rate) "BFEncodeSter" [l,r,azimuth,width,elevation,rho,gain,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  BFGrainPanner [] maxSize=0.0
bfGrainPanner :: Rate -> UGen -> UGen
bfGrainPanner rate maxSize = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "BFGrainPanner" [maxSize] Nothing 1 (Special 0) NoId

-- | BFormat sound manipulation
--
--  BFManipulate [AR] w=0.0 x=0.0 y=0.0 z=0.0 rotate=0.0 tilt=0.0 tumble=0.0
bfManipulate :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bfManipulate rate w x y z rotate_ tilt_ tumble_ = mkUGen Nothing [AR] (Left rate) "BFManipulate" [w,x,y,z,rotate_,tilt_,tumble_] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  BFPanner [] maxSize=0.0
bfPanner :: Rate -> UGen -> UGen
bfPanner rate maxSize = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "BFPanner" [maxSize] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BLBufRd [KR,AR] bufnum=0.0 phase=0.0 ratio=1.0
blBufRd :: Rate -> UGen -> UGen -> UGen -> UGen
blBufRd rate bufnum phase ratio = mkUGen Nothing [KR,AR] (Left rate) "BLBufRd" [bufnum,phase,ratio] Nothing 1 (Special 0) NoId

-- | 24db/oct rolloff - 4nd order resonant Low/High/Band Pass Filter
--
--  BMoog [AR] in=0.0 freq=440.0 q=0.2 mode=0.0 saturation=0.95;    FILTER: TRUE
bMoog :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bMoog in_ freq q mode saturation = mkUGen Nothing [AR] (Right [0]) "BMoog" [in_,freq,q,mode,saturation] Nothing 1 (Special 0) NoId

-- | Balances two signals with each other
--
--  Balance [AR] in=0.0 test=0.0 hp=10.0 stor=0.0
balance :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
balance rate in_ test hp stor = mkUGen Nothing [AR] (Left rate) "Balance" [in_,test,hp,stor] Nothing 1 (Special 0) NoId

-- | Extracts statistics on a beat histogram
--
--  BeatStatistics [KR] fft=0.0 leak=0.995 numpreviousbeats=4.0
beatStatistics :: Rate -> UGen -> UGen -> UGen -> UGen
beatStatistics rate fft_ leak numpreviousbeats = mkUGen Nothing [KR] (Left rate) "BeatStatistics" [fft_,leak,numpreviousbeats] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  BinData [KR,AR] buffer=0.0 bin=0.0 overlaps=0.5
binData :: Rate -> UGen -> UGen -> UGen -> UGen
binData rate buffer bin overlaps = mkUGen Nothing [KR,AR] (Left rate) "BinData" [buffer,bin,overlaps] Nothing 2 (Special 0) NoId

-- | Band limited impulse generation
--
--  BlitB3 [AR] freq=440.0
blitB3 :: Rate -> UGen -> UGen
blitB3 rate freq = mkUGen Nothing [AR] (Left rate) "BlitB3" [freq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BlitB3D [AR] freq=440.0
blitB3D :: Rate -> UGen -> UGen
blitB3D rate freq = mkUGen Nothing [AR] (Left rate) "BlitB3D" [freq] Nothing 1 (Special 0) NoId

-- | BLIT derived sawtooth
--
--  BlitB3Saw [AR] freq=440.0 leak=0.99
blitB3Saw :: Rate -> UGen -> UGen -> UGen
blitB3Saw rate freq leak = mkUGen Nothing [AR] (Left rate) "BlitB3Saw" [freq,leak] Nothing 1 (Special 0) NoId

-- | Bipolar BLIT derived square waveform
--
--  BlitB3Square [AR] freq=440.0 leak=0.99
blitB3Square :: Rate -> UGen -> UGen -> UGen
blitB3Square rate freq leak = mkUGen Nothing [AR] (Left rate) "BlitB3Square" [freq,leak] Nothing 1 (Special 0) NoId

-- | Bipolar BLIT derived triangle
--
--  BlitB3Tri [AR] freq=440.0 leak=0.99 leak2=0.99
blitB3Tri :: Rate -> UGen -> UGen -> UGen -> UGen
blitB3Tri rate freq leak leak2 = mkUGen Nothing [AR] (Left rate) "BlitB3Tri" [freq,leak,leak2] Nothing 1 (Special 0) NoId

-- | breakcore simulator
--
--  Breakcore [AR] bufnum=0.0 capturein=0.0 capturetrigger=0.0 duration=0.1 ampdropout=0.0
breakcore :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
breakcore rate bufnum capturein capturetrigger duration ampdropout = mkUGen Nothing [AR] (Left rate) "Breakcore" [bufnum,capturein,capturetrigger,duration,ampdropout] Nothing 1 (Special 0) NoId

-- | Prigogine oscillator
--
--  Brusselator [AR] reset=0.0 rate=1.0e-2 mu=1.0 gamma=1.0 initx=0.5 inity=0.5
brusselator :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
brusselator rate reset rate_ mu gamma initx inity = mkUGen Nothing [AR] (Left rate) "Brusselator" [reset,rate_,mu,gamma,initx,inity] Nothing 2 (Special 0) NoId

-- | Granular synthesis with sound sampled in a buffer
--
--  BufGrain [AR] trigger=0.0 dur=1.0 sndbuf=0.0 rate=1.0 pos=0.0 interp=2.0
bufGrain :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrain rate trigger dur sndbuf rate_ pos interp = mkUGen Nothing [AR] (Left rate) "BufGrain" [trigger,dur,sndbuf,rate_,pos,interp] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sound sampled in a buffer and user supplied envelope
--
--  BufGrainB [AR] trigger=0.0 dur=1.0 sndbuf=0.0 rate=1.0 pos=0.0 envbuf=0.0 interp=2.0
bufGrainB :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrainB rate trigger dur sndbuf rate_ pos envbuf interp = mkUGen Nothing [AR] (Left rate) "BufGrainB" [trigger,dur,sndbuf,rate_,pos,envbuf,interp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BufGrainBBF [AR] trigger=0.0 dur=1.0 sndbuf=0.0 rate=1.0 pos=0.0 envbuf=0.0 azimuth=0.0 elevation=0.0 rho=1.0 interp=2.0 wComp=0.0
bufGrainBBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrainBBF rate trigger dur sndbuf rate_ pos envbuf azimuth elevation rho interp wComp = mkUGen Nothing [AR] (Left rate) "BufGrainBBF" [trigger,dur,sndbuf,rate_,pos,envbuf,azimuth,elevation,rho,interp,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  BufGrainBF [AR] trigger=0.0 dur=1.0 sndbuf=0.0 rate=1.0 pos=0.0 azimuth=0.0 elevation=0.0 rho=1.0 interp=2.0 wComp=0.0
bufGrainBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrainBF rate trigger dur sndbuf rate_ pos azimuth elevation rho interp wComp = mkUGen Nothing [AR] (Left rate) "BufGrainBF" [trigger,dur,sndbuf,rate_,pos,azimuth,elevation,rho,interp,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with sound sampled in a buffer and user supplied envelopes
--
--  BufGrainI [AR] trigger=0.0 dur=1.0 sndbuf=0.0 rate=1.0 pos=0.0 envbuf1=0.0 envbuf2=0.0 ifac=0.5 interp=2.0
bufGrainI :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrainI rate trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac interp = mkUGen Nothing [AR] (Left rate) "BufGrainI" [trigger,dur,sndbuf,rate_,pos,envbuf1,envbuf2,ifac,interp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BufGrainIBF [AR] trigger=0.0 dur=1.0 sndbuf=0.0 rate=1.0 pos=0.0 envbuf1=0.0 envbuf2=0.0 ifac=0.5 azimuth=0.0 elevation=0.0 rho=1.0 interp=2.0 wComp=0.0
bufGrainIBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrainIBF rate trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac azimuth elevation rho interp wComp = mkUGen Nothing [AR] (Left rate) "BufGrainIBF" [trigger,dur,sndbuf,rate_,pos,envbuf1,envbuf2,ifac,azimuth,elevation,rho,interp,wComp] Nothing 4 (Special 0) NoId

-- | detect the largest value (and its position) in an array of UGens
--
--  BufMax [KR] bufnum=0.0 gate=1.0
bufMax :: Rate -> UGen -> UGen -> UGen
bufMax rate bufnum gate_ = mkUGen Nothing [KR] (Left rate) "BufMax" [bufnum,gate_] Nothing 2 (Special 0) NoId

-- | detect the largest value (and its position) in an array of UGens
--
--  BufMin [KR] bufnum=0.0 gate=1.0
bufMin :: Rate -> UGen -> UGen -> UGen
bufMin rate bufnum gate_ = mkUGen Nothing [KR] (Left rate) "BufMin" [bufnum,gate_] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  CQ_Diff [KR] in1=0.0 in2=0.0 databufnum=0.0
cq_Diff :: Rate -> UGen -> UGen -> UGen -> UGen
cq_Diff rate in1 in2 databufnum = mkUGen Nothing [KR] (Left rate) "CQ_Diff" [in1,in2,databufnum] Nothing 1 (Special 0) NoId

-- | Quefrency analysis and liftering
--
--  Cepstrum [] cepbuf=0.0 fftchain=0.0
cepstrum :: Rate -> UGen -> UGen -> UGen
cepstrum rate cepbuf fftchain = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "Cepstrum" [cepbuf,fftchain] Nothing 1 (Special 0) NoId

-- | Chen's chaotic double scroll attractor
--
--  Chen [KR,AR] speed=0.5 a=0.5 b=0.3 c=0.28
chen :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
chen rate speed a b c = mkUGen Nothing [KR,AR] (Left rate) "Chen" [speed,a,b,c] Nothing 3 (Special 0) NoId

-- | Octave chroma band based representation of energy in a signal; Chromagram for nTET tuning systems with any base reference
--
--  Chromagram [KR] fft=0.0 fftsize=2048.0 n=12.0 tuningbase=32.703195662575 octaves=8.0 integrationflag=0.0 coeff=0.9 octaveratio=2.0 perframenormalize=0.0
chromagram :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
chromagram rate fft_ fftsize n tuningbase octaves integrationflag coeff octaveratio perframenormalize = mkUGen Nothing [KR] (Left rate) "Chromagram" [fft_,fftsize,n,tuningbase,octaves,integrationflag,coeff,octaveratio,perframenormalize] Nothing 12 (Special 0) NoId

-- | circular linear lag
--
--  CircleRamp [KR,AR] in=0.0 lagTime=0.1 circmin=-180.0 circmax=180.0
circleRamp :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
circleRamp rate in_ lagTime circmin circmax = mkUGen Nothing [KR,AR] (Left rate) "CircleRamp" [in_,lagTime,circmin,circmax] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clipper32 [AR] in=0.0 lo=-0.8 hi=0.8
clipper32 :: Rate -> UGen -> UGen -> UGen -> UGen
clipper32 rate in_ lo hi = mkUGen Nothing [AR] (Left rate) "Clipper32" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clipper4 [AR] in=0.0 lo=-0.8 hi=0.8
clipper4 :: Rate -> UGen -> UGen -> UGen -> UGen
clipper4 rate in_ lo hi = mkUGen Nothing [AR] (Left rate) "Clipper4" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clipper8 [AR] in=0.0 lo=-0.8 hi=0.8
clipper8 :: Rate -> UGen -> UGen -> UGen -> UGen
clipper8 rate in_ lo hi = mkUGen Nothing [AR] (Left rate) "Clipper8" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clockmus [KR]
clockmus :: Rate -> UGen
clockmus rate = mkUGen Nothing [KR] (Left rate) "Clockmus" [] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  CombLP [AR] in=0.0 gate=1.0 maxdelaytime=0.2 delaytime=0.2 decaytime=1.0 coef=0.5
combLP :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
combLP rate in_ gate_ maxdelaytime delaytime decaytime coef = mkUGen Nothing [AR] (Left rate) "CombLP" [in_,gate_,maxdelaytime,delaytime,decaytime,coef] Nothing 1 (Special 0) NoId

-- | FM-modulable resonating filter
--
--  ComplexRes [AR] in=0.0 freq=100.0 decay=0.2;    FILTER: TRUE
complexRes :: UGen -> UGen -> UGen -> UGen
complexRes in_ freq decay_ = mkUGen Nothing [AR] (Right [0]) "ComplexRes" [in_,freq,decay_] Nothing 1 (Special 0) NoId

-- | Concatenative Cross-Synthesis on Live Streams
--
--  Concat [AR] control=0.0 source=0.0 storesize=1.0 seektime=1.0 seekdur=1.0 matchlength=5.0e-2 freezestore=0.0 zcr=1.0 lms=1.0 sc=1.0 st=0.0 randscore=0.0
concat :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
concat rate control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore = mkUGen Nothing [AR] (Left rate) "Concat" [control_,source,storesize,seektime,seekdur,matchlength,freezestore,zcr,lms,sc,st,randscore] Nothing 1 (Special 0) NoId

-- | Concatenative Cross-Synthesis on Live Streams
--
--  Concat2 [AR] control=0.0 source=0.0 storesize=1.0 seektime=1.0 seekdur=1.0 matchlength=5.0e-2 freezestore=0.0 zcr=1.0 lms=1.0 sc=1.0 st=0.0 randscore=0.0 threshold=1.0e-2
concat2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
concat2 rate control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore threshold = mkUGen Nothing [AR] (Left rate) "Concat2" [control_,source,storesize,seektime,seekdur,matchlength,freezestore,zcr,lms,sc,st,randscore,threshold] Nothing 1 (Special 0) NoId

-- | an amplitude tracking based onset detector
--
--  Coyote [KR] in=0.0 trackFall=0.2 slowLag=0.2 fastLag=1.0e-2 fastMul=0.5 thresh=5.0e-2 minDur=0.1
coyote :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
coyote rate in_ trackFall slowLag fastLag fastMul thresh minDur = mkUGen Nothing [KR] (Left rate) "Coyote" [in_,trackFall,slowLag,fastLag,fastMul,thresh,minDur] Nothing 1 (Special 0) NoId

-- | Measure the temporal crest factor of a signal
--
--  Crest [KR] in=0.0 numsamps=400.0 gate=1.0
crest :: Rate -> UGen -> UGen -> UGen -> UGen
crest rate in_ numsamps gate_ = mkUGen Nothing [KR] (Left rate) "Crest" [in_,numsamps,gate_] Nothing 1 (Special 0) NoId

-- | class B/AB power amp distortion simulation
--
--  CrossoverDistortion [AR] in=0.0 amp=0.5 smooth=0.5;    FILTER: TRUE
crossoverDistortion :: UGen -> UGen -> UGen -> UGen
crossoverDistortion in_ amp smooth = mkUGen Nothing [AR] (Right [0]) "CrossoverDistortion" [in_,amp,smooth] Nothing 1 (Special 0) NoId

-- | Digitally modelled analog filter
--
--  DFM1 [AR] in=0.0 freq=1000.0 res=0.1 inputgain=1.0 type=0.0 noiselevel=3.0e-4;    FILTER: TRUE
dfm1 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dfm1 in_ freq res inputgain type_ noiselevel = mkUGen Nothing [AR] (Right [0]) "DFM1" [in_,freq,res,inputgain,type_,noiselevel] Nothing 1 (Special 0) NoId

-- | Demand rate implementation of a Wiard noise ring
--
--  DNoiseRing [DR] change=0.5 chance=0.5 shift=1.0 numBits=8.0 resetval=0.0;    DEMAND/NONDET
dNoiseRing :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dNoiseRing z change chance shift numBits resetval = mkUGen Nothing [DR] (Left DR) "DNoiseRing" [change,chance,shift,numBits,resetval] Nothing 1 (Special 0) (toUId z)

-- | Triangle via 3rd order differerentiated polynomial waveform
--
--  DPW3Tri [AR] freq=440.0
dpw3Tri :: Rate -> UGen -> UGen
dpw3Tri rate freq = mkUGen Nothing [AR] (Left rate) "DPW3Tri" [freq] Nothing 1 (Special 0) NoId

-- | Sawtooth via 4th order differerentiated polynomial waveform
--
--  DPW4Saw [AR] freq=440.0
dpw4Saw :: Rate -> UGen -> UGen
dpw4Saw rate freq = mkUGen Nothing [AR] (Left rate) "DPW4Saw" [freq] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGBowed [AR] freq=440.0 velb=0.5 force=1.0 gate=1.0 pos=0.14 release=0.1 c1=1.0 c3=3.0 impZ=0.55 fB=2.0
dwgBowed :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgBowed rate freq velb force gate_ pos release c1 c3 impZ fB = mkUGen Nothing [AR] (Left rate) "DWGBowed" [freq,velb,force,gate_,pos,release,c1,c3,impZ,fB] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGBowedSimple [AR] freq=440.0 velb=0.5 force=1.0 gate=1.0 pos=0.14 release=0.1 c1=1.0 c3=30.0
dwgBowedSimple :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgBowedSimple rate freq velb force gate_ pos release c1 c3 = mkUGen Nothing [AR] (Left rate) "DWGBowedSimple" [freq,velb,force,gate_,pos,release,c1,c3] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGBowedTor [AR] freq=440.0 velb=0.5 force=1.0 gate=1.0 pos=0.14 release=0.1 c1=1.0 c3=3.0 impZ=0.55 fB=2.0 mistune=5.2 c1tor=1.0 c3tor=3000.0 iZtor=1.8
dwgBowedTor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgBowedTor rate freq velb force gate_ pos release c1 c3 impZ fB mistune c1tor c3tor iZtor = mkUGen Nothing [AR] (Left rate) "DWGBowedTor" [freq,velb,force,gate_,pos,release,c1,c3,impZ,fB,mistune,c1tor,c3tor,iZtor] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGPlucked [AR] freq=440.0 amp=0.5 gate=1.0 pos=0.14 c1=1.0 c3=30.0 inp=0.0 release=0.1
dwgPlucked :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgPlucked rate freq amp gate_ pos c1 c3 inp release = mkUGen Nothing [AR] (Left rate) "DWGPlucked" [freq,amp,gate_,pos,c1,c3,inp,release] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGPlucked2 [AR] freq=440.0 amp=0.5 gate=1.0 pos=0.14 c1=1.0 c3=30.0 inp=0.0 release=0.1 mistune=1.008 mp=0.55 gc=1.0e-2
dwgPlucked2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgPlucked2 rate freq amp gate_ pos c1 c3 inp release mistune mp gc = mkUGen Nothing [AR] (Left rate) "DWGPlucked2" [freq,amp,gate_,pos,c1,c3,inp,release,mistune,mp,gc] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGPluckedStiff [AR] freq=440.0 amp=0.5 gate=1.0 pos=0.14 c1=1.0 c3=30.0 inp=0.0 release=0.1 fB=2.0
dwgPluckedStiff :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgPluckedStiff rate freq amp gate_ pos c1 c3 inp release fB = mkUGen Nothing [AR] (Left rate) "DWGPluckedStiff" [freq,amp,gate_,pos,c1,c3,inp,release,fB] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  DWGSoundBoard [AR] inp=0.0 c1=20.0 c3=20.0 mix=0.8 d1=199.0 d2=211.0 d3=223.0 d4=227.0 d5=229.0 d6=233.0 d7=239.0 d8=241.0
dwgSoundBoard :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgSoundBoard rate inp c1 c3 mix d1 d2 d3 d4 d5 d6 d7 d8 = mkUGen Nothing [AR] (Left rate) "DWGSoundBoard" [inp,c1,c3,mix,d1,d2,d3,d4,d5,d6,d7,d8] Nothing 1 (Special 0) NoId

-- | demand rate brownian movement with Gendyn distributions
--
--  Dbrown2 [] lo=0.0 hi=0.0 step=0.0 dist=0.0 length=1.0e8
dbrown2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dbrown2 rate lo hi step dist length_ = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "Dbrown2" [lo,hi,step,dist,length_] Nothing 1 (Special 0) NoId

-- | demand rate tag system on a buffer
--
--  DbufTag [DR] bufnum=0.0 v=0.0 axiom=0.0 rules=0.0 recycle=0.0 mode=0.0;    DEMAND/NONDET
dbufTag :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dbufTag z bufnum v axiom rules recycle mode = mkUGen Nothing [DR] (Left DR) "DbufTag" [bufnum,v,axiom,rules,recycle,mode] Nothing 1 (Special 0) (toUId z)

-- | Samplerate and bitrate reduction
--
--  Decimator [AR] in=0.0 rate=44100.0 bits=24.0
decimator :: Rate -> UGen -> UGen -> UGen -> UGen
decimator rate in_ rate_ bits = mkUGen Nothing [AR] (Left rate) "Decimator" [in_,rate_,bits] Nothing 1 (Special 0) NoId

-- | Demand version of the BetaBlocker VChip
--
--  DetaBlockerBuf [DR] bufnum=0.0 startpoint=0.0;    DEMAND/NONDET
detaBlockerBuf :: ID a => a -> UGen -> UGen -> UGen
detaBlockerBuf z bufnum startpoint = mkUGen Nothing [DR] (Left DR) "DetaBlockerBuf" [bufnum,startpoint] Nothing 1 (Special 0) (toUId z)

-- | demand rate finite state machine
--
--  Dfsm [DR] rules=0.0 n=1.0 rgen=0.0;    DEMAND/NONDET
dfsm :: ID a => a -> UGen -> UGen -> UGen -> UGen
dfsm z rules n rgen = mkUGen Nothing [DR] (Left DR) "Dfsm" [rules,n,rgen] Nothing 1 (Special 0) (toUId z)

-- | (Undocumented class)
--
--  Dgauss [] lo=0.0 hi=0.0 length=1.0e8
dgauss :: Rate -> UGen -> UGen -> UGen -> UGen
dgauss rate lo hi length_ = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "Dgauss" [lo,hi,length_] Nothing 1 (Special 0) NoId

-- | Ring modulation based on the physical model of a diode.
--
--  DiodeRingMod [AR] car=0.0 mod=0.0;    FILTER: TRUE
diodeRingMod :: UGen -> UGen -> UGen
diodeRingMod car mod_ = mkUGen Nothing [AR] (Right [0]) "DiodeRingMod" [car,mod_] Nothing 1 (Special 0) NoId

-- | port of some ladspa plugins
--
--  Disintegrator [AR] in=0.0 probability=0.5 multiplier=0.0;    FILTER: TRUE, NONDET
disintegrator :: ID a => a -> UGen -> UGen -> UGen -> UGen
disintegrator z in_ probability multiplier = mkUGen Nothing [AR] (Right [0]) "Disintegrator" [in_,probability,multiplier] Nothing 1 (Special 0) (toUId z)

-- | discrete time neurodynamics
--
--  Dneuromodule [KR,AR,DR] dt=0.0 *theta=0.0 *x=0.0 *weights=0.0;    MCE=3, NC INPUT: True, NONDET
dneuromodule :: ID a => Int -> a -> UGen -> UGen -> UGen -> UGen -> UGen
dneuromodule numChannels z dt theta x weights = mkUGen Nothing [KR,AR,DR] (Left DR) "Dneuromodule" [dt] (Just [theta,x,weights]) numChannels (Special 0) (toUId z)

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  DoubleNestedAllpassC [AR] in=0.0 maxdelay1=4.7e-3 delay1=4.7e-3 gain1=0.15 maxdelay2=2.2e-2 delay2=2.2e-2 gain2=0.25 maxdelay3=8.3e-3 delay3=8.3e-3 gain3=0.3;    FILTER: TRUE
doubleNestedAllpassC :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleNestedAllpassC in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 maxdelay3 delay3 gain3 = mkUGen Nothing [AR] (Right [0]) "DoubleNestedAllpassC" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2,maxdelay3,delay3,gain3] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  DoubleNestedAllpassL [AR] in=0.0 maxdelay1=4.7e-3 delay1=4.7e-3 gain1=0.15 maxdelay2=2.2e-2 delay2=2.2e-2 gain2=0.25 maxdelay3=8.3e-3 delay3=8.3e-3 gain3=0.3;    FILTER: TRUE
doubleNestedAllpassL :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleNestedAllpassL in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 maxdelay3 delay3 gain3 = mkUGen Nothing [AR] (Right [0]) "DoubleNestedAllpassL" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2,maxdelay3,delay3,gain3] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  DoubleNestedAllpassN [AR] in=0.0 maxdelay1=4.7e-3 delay1=4.7e-3 gain1=0.15 maxdelay2=2.2e-2 delay2=2.2e-2 gain2=0.25 maxdelay3=8.3e-3 delay3=8.3e-3 gain3=0.3;    FILTER: TRUE
doubleNestedAllpassN :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleNestedAllpassN in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 maxdelay3 delay3 gain3 = mkUGen Nothing [AR] (Right [0]) "DoubleNestedAllpassN" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2,maxdelay3,delay3,gain3] Nothing 1 (Special 0) NoId

-- | Forced DoubleWell Oscillator
--
--  DoubleWell [AR] reset=0.0 ratex=1.0e-2 ratey=1.0e-2 f=1.0 w=1.0e-3 delta=1.0 initx=0.0 inity=0.0
doubleWell :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleWell rate reset ratex ratey f w delta initx inity = mkUGen Nothing [AR] (Left rate) "DoubleWell" [reset,ratex,ratey,f,w,delta,initx,inity] Nothing 1 (Special 0) NoId

-- | Forced DoubleWell Oscillator
--
--  DoubleWell2 [AR] reset=0.0 ratex=1.0e-2 ratey=1.0e-2 f=1.0 w=1.0e-3 delta=1.0 initx=0.0 inity=0.0
doubleWell2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleWell2 rate reset ratex ratey f w delta initx inity = mkUGen Nothing [AR] (Left rate) "DoubleWell2" [reset,ratex,ratey,f,w,delta,initx,inity] Nothing 1 (Special 0) NoId

-- | Forced DoubleWell Oscillator
--
--  DoubleWell3 [AR] reset=0.0 rate=1.0e-2 f=0.0 delta=0.25 initx=0.0 inity=0.0
doubleWell3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleWell3 rate reset rate_ f delta initx inity = mkUGen Nothing [AR] (Left rate) "DoubleWell3" [reset,rate_,f,delta,initx,inity] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  DriveNoise [AR] in=0.0 amount=1.0 multi=5.0
driveNoise :: Rate -> UGen -> UGen -> UGen -> UGen
driveNoise rate in_ amount multi = mkUGen Nothing [AR] (Left rate) "DriveNoise" [in_,amount,multi] Nothing 1 (Special 0) NoId

-- | Crosscorrelation search and drum pattern matching beat tracker
--
--  DrumTrack [KR] in=0.0 lock=0.0 dynleak=0.0 tempowt=0.0 phasewt=0.0 basswt=0.0 patternwt=1.0 prior=0.0 kicksensitivity=1.0 snaresensitivity=1.0 debugmode=0.0
drumTrack :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
drumTrack rate in_ lock dynleak tempowt phasewt basswt patternwt prior kicksensitivity snaresensitivity debugmode = mkUGen Nothing [KR] (Left rate) "DrumTrack" [in_,lock,dynleak,tempowt,phasewt,basswt,patternwt,prior,kicksensitivity,snaresensitivity,debugmode] Nothing 4 (Special 0) NoId

-- | demand rate tag system
--
--  Dtag [] bufsize=0.0 v=0.0 axiom=0.0 rules=0.0 recycle=0.0 mode=0.0
dtag :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dtag rate bufsize v axiom rules recycle mode = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "Dtag" [bufsize,v,axiom,rules,recycle,mode] Nothing 1 (Special 0) NoId

-- | Envelope Follower Filter
--
--  EnvDetect [AR] in=0.0 attack=100.0 release=0.0
envDetect :: Rate -> UGen -> UGen -> UGen -> UGen
envDetect rate in_ attack release = mkUGen Nothing [AR] (Left rate) "EnvDetect" [in_,attack,release] Nothing 1 (Special 0) NoId

-- | Envelope Follower
--
--  EnvFollow [KR,AR] input=0.0 decaycoeff=0.99
envFollow :: Rate -> UGen -> UGen -> UGen
envFollow rate input decaycoeff = mkUGen Nothing [KR,AR] (Left rate) "EnvFollow" [input,decaycoeff] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTComplexDev [KR] buffer=0.0 rectify=0.0 powthresh=0.1
fftComplexDev :: Rate -> UGen -> UGen -> UGen -> UGen
fftComplexDev rate buffer rectify powthresh = mkUGen Nothing [KR] (Left rate) "FFTComplexDev" [buffer,rectify,powthresh] Nothing 1 (Special 0) NoId

-- | Spectral crest measure
--
--  FFTCrest [KR] buffer=0.0 freqlo=0.0 freqhi=50000.0
fftCrest :: Rate -> UGen -> UGen -> UGen -> UGen
fftCrest rate buffer freqlo freqhi = mkUGen Nothing [KR] (Left rate) "FFTCrest" [buffer,freqlo,freqhi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTDiffMags [KR] bufferA=0.0 bufferB=0.0
fftDiffMags :: Rate -> UGen -> UGen -> UGen
fftDiffMags rate bufferA bufferB = mkUGen Nothing [KR] (Left rate) "FFTDiffMags" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTFlux [KR] buffer=0.0 normalise=1.0
fftFlux :: Rate -> UGen -> UGen -> UGen
fftFlux rate buffer normalise = mkUGen Nothing [KR] (Left rate) "FFTFlux" [buffer,normalise] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTFluxPos [KR] buffer=0.0 normalise=1.0
fftFluxPos :: Rate -> UGen -> UGen -> UGen
fftFluxPos rate buffer normalise = mkUGen Nothing [KR] (Left rate) "FFTFluxPos" [buffer,normalise] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTMKL [KR] buffer=0.0 epsilon=1.0e-6
fftmkl :: Rate -> UGen -> UGen -> UGen
fftmkl rate buffer epsilon = mkUGen Nothing [KR] (Left rate) "FFTMKL" [buffer,epsilon] Nothing 1 (Special 0) NoId

-- | Find peak value in an FFT frame
--
--  FFTPeak [KR] buffer=0.0 freqlo=0.0 freqhi=50000.0
fftPeak :: Rate -> UGen -> UGen -> UGen -> UGen
fftPeak rate buffer freqlo freqhi = mkUGen Nothing [KR] (Left rate) "FFTPeak" [buffer,freqlo,freqhi] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTPhaseDev [KR] buffer=0.0 weight=0.0 powthresh=0.1
fftPhaseDev :: Rate -> UGen -> UGen -> UGen -> UGen
fftPhaseDev rate buffer weight powthresh = mkUGen Nothing [KR] (Left rate) "FFTPhaseDev" [buffer,weight,powthresh] Nothing 1 (Special 0) NoId

-- | Instantaneous spectral power
--
--  FFTPower [KR] buffer=0.0 square=1.0
fftPower :: Rate -> UGen -> UGen -> UGen
fftPower rate buffer square = mkUGen Nothing [KR] (Left rate) "FFTPower" [buffer,square] Nothing 1 (Special 0) NoId

-- | Spectral slope
--
--  FFTSlope [KR] buffer=0.0
fftSlope :: Rate -> UGen -> UGen
fftSlope rate buffer = mkUGen Nothing [KR] (Left rate) "FFTSlope" [buffer] Nothing 1 (Special 0) NoId

-- | Spectral spread
--
--  FFTSpread [KR] buffer=0.0 centroid=0.0
fftSpread :: Rate -> UGen -> UGen -> UGen
fftSpread rate buffer centroid = mkUGen Nothing [KR] (Left rate) "FFTSpread" [buffer,centroid] Nothing 1 (Special 0) NoId

-- | Spectral flatness, divided into subbands
--
--  FFTSubbandFlatness [KR] chain=0.0 cutfreqs=0.0
fftSubbandFlatness :: Rate -> UGen -> UGen -> UGen
fftSubbandFlatness rate chain cutfreqs = mkUGen Nothing [KR] (Left rate) "FFTSubbandFlatness" [chain,cutfreqs] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTSubbandFlux [KR] chain=0.0 cutfreqs=0.0 posonly=0.0
fftSubbandFlux :: Rate -> UGen -> UGen -> UGen -> UGen
fftSubbandFlux rate chain cutfreqs posonly = mkUGen Nothing [KR] (Left rate) "FFTSubbandFlux" [chain,cutfreqs,posonly] Nothing 1 (Special 0) NoId

-- | Spectral power, divided into subbands
--
--  FFTSubbandPower [KR] chain=0.0 cutfreqs=0.0 square=1.0 scalemode=1.0
fftSubbandPower :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
fftSubbandPower rate chain cutfreqs square scalemode = mkUGen Nothing [KR] (Left rate) "FFTSubbandPower" [chain,cutfreqs,square,scalemode] Nothing 1 (Special 0) NoId

-- | Phase modulation oscillator matrix.
--
--  FM7 [AR] *ctlMatrix=0.0 *modMatrix=0.0;    MCE=2
fm7 :: Rate -> UGen -> UGen -> UGen
fm7 rate ctlMatrix modMatrix = mkUGen Nothing [AR] (Left rate) "FM7" [] (Just [ctlMatrix,modMatrix]) 6 (Special 0) NoId

-- | Granular synthesis with FM grains
--
--  FMGrain [AR] trigger=0.0 dur=1.0 carfreq=440.0 modfreq=200.0 index=1.0;    FILTER: TRUE
fmGrain :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrain trigger dur carfreq modfreq index_ = mkUGen Nothing [AR] (Right [0]) "FMGrain" [trigger,dur,carfreq,modfreq,index_] Nothing 1 (Special 0) NoId

-- | Granular synthesis with FM grains and user supplied envelope
--
--  FMGrainB [AR] trigger=0.0 dur=1.0 carfreq=440.0 modfreq=200.0 index=1.0 envbuf=0.0;    FILTER: TRUE
fmGrainB :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainB trigger dur carfreq modfreq index_ envbuf = mkUGen Nothing [AR] (Right [0]) "FMGrainB" [trigger,dur,carfreq,modfreq,index_,envbuf] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FMGrainBBF [AR] trigger=0.0 dur=1.0 carfreq=440.0 modfreq=200.0 index=1.0 envbuf=0.0 azimuth=0.0 elevation=0.0 rho=1.0 wComp=0.0
fmGrainBBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainBBF rate trigger dur carfreq modfreq index_ envbuf azimuth elevation rho wComp = mkUGen Nothing [AR] (Left rate) "FMGrainBBF" [trigger,dur,carfreq,modfreq,index_,envbuf,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  FMGrainBF [AR] trigger=0.0 dur=1.0 carfreq=440.0 modfreq=200.0 index=1.0 azimuth=0.0 elevation=0.0 rho=1.0 wComp=0.0
fmGrainBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainBF rate trigger dur carfreq modfreq index_ azimuth elevation rho wComp = mkUGen Nothing [AR] (Left rate) "FMGrainBF" [trigger,dur,carfreq,modfreq,index_,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with FM grains and user supplied envelopes
--
--  FMGrainI [AR] trigger=0.0 dur=1.0 carfreq=440.0 modfreq=200.0 index=1.0 envbuf1=0.0 envbuf2=0.0 ifac=0.5
fmGrainI :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainI rate trigger dur carfreq modfreq index_ envbuf1 envbuf2 ifac = mkUGen Nothing [AR] (Left rate) "FMGrainI" [trigger,dur,carfreq,modfreq,index_,envbuf1,envbuf2,ifac] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FMGrainIBF [AR] trigger=0.0 dur=1.0 carfreq=440.0 modfreq=200.0 index=1.0 envbuf1=0.0 envbuf2=0.0 ifac=0.5 azimuth=0.0 elevation=0.0 rho=1.0 wComp=0.0
fmGrainIBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainIBF rate trigger dur carfreq modfreq index_ envbuf1 envbuf2 ifac azimuth elevation rho wComp = mkUGen Nothing [AR] (Left rate) "FMGrainIBF" [trigger,dur,carfreq,modfreq,index_,envbuf1,envbuf2,ifac,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Decode an FMH signal for a specific speaker
--
--  FMHDecode1 [AR] w=0.0 x=0.0 y=0.0 z=0.0 r=0.0 s=0.0 t=0.0 u=0.0 v=0.0 azimuth=0.0 elevation=0.0
fmhDecode1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmhDecode1 rate w x y z r s t u v azimuth elevation = mkUGen Nothing [AR] (Left rate) "FMHDecode1" [w,x,y,z,r,s,t,u,v,azimuth,elevation] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FMHEncode0 [AR] in=0.0 azimuth=0.0 elevation=0.0 gain=1.0
fmhEncode0 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
fmhEncode0 rate in_ azimuth elevation gain = mkUGen Nothing [AR] (Left rate) "FMHEncode0" [in_,azimuth,elevation,gain] Nothing 9 (Special 0) NoId

-- | Second Order Ambisonic encoder
--
--  FMHEncode1 [AR] in=0.0 azimuth=0.0 elevation=0.0 rho=1.0 gain=1.0 wComp=0.0
fmhEncode1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmhEncode1 rate in_ azimuth elevation rho gain wComp = mkUGen Nothing [AR] (Left rate) "FMHEncode1" [in_,azimuth,elevation,rho,gain,wComp] Nothing 9 (Special 0) NoId

-- | Second Order Ambisonic encoder
--
--  FMHEncode2 [AR] in=0.0 point_x=0.0 point_y=0.0 elevation=0.0 gain=1.0 wComp=0.0
fmhEncode2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmhEncode2 rate in_ point_x point_y elevation gain wComp = mkUGen Nothing [AR] (Left rate) "FMHEncode2" [in_,point_x,point_y,elevation,gain,wComp] Nothing 9 (Special 0) NoId

-- | Storing feature data from UGens in NRT mode
--
--  FeatureSave [KR] features=0.0 trig=0.0
featureSave :: Rate -> UGen -> UGen -> UGen
featureSave rate features trig_ = mkUGen Nothing [KR] (Left rate) "FeatureSave" [features,trig_] Nothing 1 (Special 0) NoId

-- | FitzHughNagumo Neuron Firing Oscillator
--
--  Fhn2DC [KR,AR] minfreq=11025.0 maxfreq=22050.0 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0.0 u0=0.0 w0=0.0
fhn2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fhn2DC rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUGen Nothing [KR,AR] (Left rate) "Fhn2DC" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | FitzHughNagumo Neuron Firing Oscillator
--
--  Fhn2DL [KR,AR] minfreq=11025.0 maxfreq=22050.0 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0.0 u0=0.0 w0=0.0
fhn2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fhn2DL rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUGen Nothing [KR,AR] (Left rate) "Fhn2DL" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | FitzHughNagumo Neuron Firing Oscillator
--
--  Fhn2DN [KR,AR] minfreq=11025.0 maxfreq=22050.0 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0.0 u0=0.0 w0=0.0
fhn2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fhn2DN rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUGen Nothing [KR,AR] (Left rate) "Fhn2DN" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FhnTrig [KR,AR] minfreq=4.0 maxfreq=10.0 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0.0 u0=0.0 w0=0.0
fhnTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fhnTrig rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUGen Nothing [KR,AR] (Left rate) "FhnTrig" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FincoSprottL [AR] freq=22050.0 a=2.45 h=5.0e-2 xi=0.0 yi=0.0 zi=0.0
fincoSprottL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fincoSprottL rate freq a h xi yi zi = mkUGen Nothing [AR] (Left rate) "FincoSprottL" [freq,a,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  FincoSprottM [AR] freq=22050.0 a=-7.0 b=4.0 h=5.0e-2 xi=0.0 yi=0.0 zi=0.0
fincoSprottM :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fincoSprottM rate freq a b h xi yi zi = mkUGen Nothing [AR] (Left rate) "FincoSprottM" [freq,a,b,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  FincoSprottS [AR] freq=22050.0 a=8.0 b=2.0 h=5.0e-2 xi=0.0 yi=0.0 zi=0.0
fincoSprottS :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fincoSprottS rate freq a b h xi yi zi = mkUGen Nothing [AR] (Left rate) "FincoSprottS" [freq,a,b,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | Neuron Firing Model Oscillator
--
--  FitzHughNagumo [AR] reset=0.0 rateu=1.0e-2 ratew=1.0e-2 b0=1.0 b1=1.0 initu=0.0 initw=0.0
fitzHughNagumo :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fitzHughNagumo rate reset rateu ratew b0 b1 initu initw = mkUGen Nothing [AR] (Left rate) "FitzHughNagumo" [reset,rateu,ratew,b0,b1,initu,initw] Nothing 1 (Special 0) NoId

-- | calculates spectral MSE distance of two fft chains
--
--  FrameCompare [KR] buffer1=0.0 buffer2=0.0 wAmount=0.5
frameCompare :: Rate -> UGen -> UGen -> UGen -> UGen
frameCompare rate buffer1 buffer2 wAmount = mkUGen Nothing [KR] (Left rate) "FrameCompare" [buffer1,buffer2,wAmount] Nothing 1 (Special 0) NoId

-- | A physical model of a system with dry-friction. A chaotic filter.
--
--  Friction [KR,AR] in=0.0 friction=0.5 spring=0.414 damp=0.313 mass=0.1 beltmass=1.0
friction :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
friction rate in_ friction_ spring_ damp mass beltmass = mkUGen Nothing [KR,AR] (Left rate) "Friction" [in_,friction_,spring_,damp,mass,beltmass] Nothing 1 (Special 0) NoId

-- | Single gammatone filter
--
--  Gammatone [AR] input=0.0 centrefrequency=440.0 bandwidth=200.0;    FILTER: TRUE
gammatone :: UGen -> UGen -> UGen -> UGen
gammatone input centrefrequency bandwidth = mkUGen Nothing [AR] (Right [0]) "Gammatone" [input,centrefrequency,bandwidth] Nothing 1 (Special 0) NoId

-- | Gaussian classifier
--
--  GaussClass [KR] in=0.0 bufnum=0.0 gate=0.0
gaussClass :: Rate -> UGen -> UGen -> UGen -> UGen
gaussClass rate in_ bufnum gate_ = mkUGen Nothing [KR] (Left rate) "GaussClass" [in_,bufnum,gate_] Nothing 1 (Special 0) NoId

-- | impulses around a certain frequency
--
--  GaussTrig [KR,AR] freq=440.0 dev=0.3
gaussTrig :: Rate -> UGen -> UGen -> UGen
gaussTrig rate freq dev = mkUGen Nothing [KR,AR] (Left rate) "GaussTrig" [freq,dev] Nothing 1 (Special 0) NoId

-- | gingerbreadman map 2D chaotic generator
--
--  Gbman2DC [KR,AR] minfreq=11025.0 maxfreq=22050.0 x0=1.2 y0=2.1
gbman2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
gbman2DC rate minfreq maxfreq x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Gbman2DC" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | gingerbreadman map 2D chaotic generator
--
--  Gbman2DL [KR,AR] minfreq=11025.0 maxfreq=22050.0 x0=1.2 y0=2.1
gbman2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
gbman2DL rate minfreq maxfreq x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Gbman2DL" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | gingerbreadman map 2D chaotic generator
--
--  Gbman2DN [KR,AR] minfreq=11025.0 maxfreq=22050.0 x0=1.2 y0=2.1
gbman2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
gbman2DN rate minfreq maxfreq x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Gbman2DN" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GbmanTrig [KR,AR] minfreq=5.0 maxfreq=10.0 x0=1.2 y0=2.1
gbmanTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
gbmanTrig rate minfreq maxfreq x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "GbmanTrig" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | Dynamic stochastic synthesis generator
--
--  Gendy4 [KR,AR] ampdist=1.0 durdist=1.0 adparam=1.0 ddparam=1.0 minfreq=440.0 maxfreq=660.0 ampscale=0.5 durscale=0.5 initCPs=12.0 knum=0.0
gendy4 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy4 rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum = mkUGen Nothing [KR,AR] (Left rate) "Gendy4" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) NoId

-- | Dynamic stochastic synthesis generator
--
--  Gendy5 [KR,AR] ampdist=1.0 durdist=1.0 adparam=1.0 ddparam=1.0 minfreq=440.0 maxfreq=660.0 ampscale=0.5 durscale=0.5 initCPs=12.0 knum=0.0
gendy5 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy5 rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum = mkUGen Nothing [KR,AR] (Left rate) "Gendy5" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) NoId

-- | Read (numeric) shell environment variables into a synth
--
--  Getenv [] key=0.0 defaultval=0.0
getenv :: Rate -> UGen -> UGen -> UGen
getenv rate key defaultval = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "Getenv" [key,defaultval] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchBPF [KR,AR] in=0.0 freq=440.0 rq=1.0
glitchBPF :: Rate -> UGen -> UGen -> UGen -> UGen
glitchBPF rate in_ freq rq = mkUGen Nothing [KR,AR] (Left rate) "GlitchBPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchBRF [KR,AR] in=0.0 freq=440.0 rq=1.0
glitchBRF :: Rate -> UGen -> UGen -> UGen -> UGen
glitchBRF rate in_ freq rq = mkUGen Nothing [KR,AR] (Left rate) "GlitchBRF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchHPF [KR,AR] in=0.0 freq=440.0
glitchHPF :: Rate -> UGen -> UGen -> UGen
glitchHPF rate in_ freq = mkUGen Nothing [KR,AR] (Left rate) "GlitchHPF" [in_,freq] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchRHPF [KR,AR] in=0.0 freq=440.0 rq=1.0
glitchRHPF :: Rate -> UGen -> UGen -> UGen -> UGen
glitchRHPF rate in_ freq rq = mkUGen Nothing [KR,AR] (Left rate) "GlitchRHPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Calculate a single DFT bin, to detect presence of a frequency
--
--  Goertzel [KR] in=0.0 bufsize=1024.0 freq=0.0 hop=1.0
goertzel :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
goertzel rate in_ bufsize freq hop = mkUGen Nothing [KR] (Left rate) "Goertzel" [in_,bufsize,freq,hop] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainBufJ [AR] numChannels=1.0 trigger=0.0 dur=1.0 sndbuf=0.0 rate=1.0 pos=0.0 loop=0.0 interp=2.0 grainAmp=1.0 pan=0.0 envbufnum=-1.0 maxGrains=512.0
grainBufJ :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainBufJ rate numChannels trigger dur sndbuf rate_ pos loop interp grainAmp pan envbufnum maxGrains = mkUGen Nothing [AR] (Left rate) "GrainBufJ" [numChannels,trigger,dur,sndbuf,rate_,pos,loop,interp,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainFMJ [AR] numChannels=1.0 trigger=0.0 dur=1.0 carfreq=440.0 modfreq=200.0 index=1.0 grainAmp=1.0 pan=0.0 envbufnum=-1.0 maxGrains=512.0
grainFMJ :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainFMJ rate numChannels trigger dur carfreq modfreq index_ grainAmp pan envbufnum maxGrains = mkUGen Nothing [AR] (Left rate) "GrainFMJ" [numChannels,trigger,dur,carfreq,modfreq,index_,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainInJ [AR] numChannels=1.0 trigger=0.0 dur=1.0 in=0.0 grainAmp=1.0 pan=0.0 envbufnum=-1.0 maxGrains=512.0
grainInJ :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainInJ rate numChannels trigger dur in_ grainAmp pan envbufnum maxGrains = mkUGen Nothing [AR] (Left rate) "GrainInJ" [numChannels,trigger,dur,in_,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainSinJ [AR] numChannels=1.0 trigger=0.0 dur=1.0 freq=440.0 grainAmp=1.0 pan=0.0 envbufnum=-1.0 maxGrains=512.0
grainSinJ :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainSinJ rate numChannels trigger dur freq grainAmp pan envbufnum maxGrains = mkUGen Nothing [AR] (Left rate) "GrainSinJ" [numChannels,trigger,dur,freq,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | dynamical system simulation (Newtonian gravitational force)
--
--  GravityGrid [AR] reset=0.0 rate=0.1 newx=0.0 newy=0.0 bufnum=0.0
gravityGrid :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gravityGrid rate reset rate_ newx newy bufnum = mkUGen Nothing [AR] (Left rate) "GravityGrid" [reset,rate_,newx,newy,bufnum] Nothing 1 (Special 0) NoId

-- | dynamical system simulation (Newtonian gravitational force)
--
--  GravityGrid2 [AR] reset=0.0 rate=0.1 newx=0.0 newy=0.0 bufnum=0.0
gravityGrid2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gravityGrid2 rate reset rate_ newx newy bufnum = mkUGen Nothing [AR] (Left rate) "GravityGrid2" [reset,rate_,newx,newy,bufnum] Nothing 1 (Special 0) NoId

-- | algorithmic delay
--
--  GreyholeRaw [AR] in1=0.0 in2=0.0 damping=0.0 delaytime=2.0 diffusion=0.5 feedback=0.9 moddepth=0.1 modfreq=2.0 size=1.0;    FILTER: TRUE
greyholeRaw :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
greyholeRaw in1 in2 damping delaytime diffusion feedback moddepth modfreq size = mkUGen Nothing [AR] (Right [0,1]) "GreyholeRaw" [in1,in2,damping,delaytime,diffusion,feedback,moddepth,modfreq,size] Nothing 2 (Special 0) NoId

-- | Simple cochlear hair cell model
--
--  HairCell [KR,AR] input=0.0 spontaneousrate=0.0 boostrate=200.0 restorerate=1000.0 loss=0.99;    FILTER: TRUE
hairCell :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
hairCell input spontaneousrate boostrate restorerate loss = mkUGen Nothing [KR,AR] (Right [0]) "HairCell" [input,spontaneousrate,boostrate,restorerate,loss] Nothing 1 (Special 0) NoId

-- | henon map 2D chaotic generator
--
--  Henon2DC [KR,AR] minfreq=11025.0 maxfreq=22050.0 a=1.4 b=0.3 x0=0.30501993062401 y0=0.20938865431933
henon2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henon2DC rate minfreq maxfreq a b x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Henon2DC" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | henon map 2D chaotic generator
--
--  Henon2DL [KR,AR] minfreq=11025.0 maxfreq=22050.0 a=1.4 b=0.3 x0=0.30501993062401 y0=0.20938865431933
henon2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henon2DL rate minfreq maxfreq a b x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Henon2DL" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | henon map 2D chaotic generator
--
--  Henon2DN [KR,AR] minfreq=11025.0 maxfreq=22050.0 a=1.4 b=0.3 x0=0.30501993062401 y0=0.20938865431933
henon2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henon2DN rate minfreq maxfreq a b x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Henon2DN" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  HenonTrig [KR,AR] minfreq=5.0 maxfreq=10.0 a=1.4 b=0.3 x0=0.30501993062401 y0=0.20938865431933
henonTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonTrig rate minfreq maxfreq a b x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "HenonTrig" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | Transform a cepstrum back to a spectrum
--
--  ICepstrum [] cepchain=0.0 fftbuf=0.0
iCepstrum :: Rate -> UGen -> UGen -> UGen
iCepstrum rate cepchain fftbuf = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "ICepstrum" [cepchain,fftbuf] Nothing 1 (Special 0) NoId

-- | 24db/oct rolloff, 4nd order resonant Low Pass Filter
--
--  IIRFilter [AR] in=0.0 freq=440.0 rq=1.0;    FILTER: TRUE
iirFilter :: UGen -> UGen -> UGen -> UGen
iirFilter in_ freq rq = mkUGen Nothing [AR] (Right [0]) "IIRFilter" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrain [AR] trigger=0.0 dur=1.0 in=0.0
inGrain :: Rate -> UGen -> UGen -> UGen -> UGen
inGrain rate trigger dur in_ = mkUGen Nothing [AR] (Left rate) "InGrain" [trigger,dur,in_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainB [AR] trigger=0.0 dur=1.0 in=0.0 envbuf=0.0
inGrainB :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
inGrainB rate trigger dur in_ envbuf = mkUGen Nothing [AR] (Left rate) "InGrainB" [trigger,dur,in_,envbuf] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainBBF [AR] trigger=0.0 dur=1.0 in=0.0 envbuf=0.0 azimuth=0.0 elevation=0.0 rho=1.0 wComp=0.0
inGrainBBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
inGrainBBF rate trigger dur in_ envbuf azimuth elevation rho wComp = mkUGen Nothing [AR] (Left rate) "InGrainBBF" [trigger,dur,in_,envbuf,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainBF [AR] trigger=0.0 dur=1.0 in=0.0 azimuth=0.0 elevation=0.0 rho=1.0 wComp=0.0
inGrainBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
inGrainBF rate trigger dur in_ azimuth elevation rho wComp = mkUGen Nothing [AR] (Left rate) "InGrainBF" [trigger,dur,in_,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainI [AR] trigger=0.0 dur=1.0 in=0.0 envbuf1=0.0 envbuf2=0.0 ifac=0.5
inGrainI :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
inGrainI rate trigger dur in_ envbuf1 envbuf2 ifac = mkUGen Nothing [AR] (Left rate) "InGrainI" [trigger,dur,in_,envbuf1,envbuf2,ifac] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainIBF [AR] trigger=0.0 dur=1.0 in=0.0 envbuf1=0.0 envbuf2=0.0 ifac=0.5 azimuth=0.0 elevation=0.0 rho=1.0 wComp=0.0
inGrainIBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
inGrainIBF rate trigger dur in_ envbuf1 envbuf2 ifac azimuth elevation rho wComp = mkUGen Nothing [AR] (Left rate) "InGrainIBF" [trigger,dur,in_,envbuf1,envbuf2,ifac,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Distortion by subtracting magnitude from 1
--
--  InsideOut [KR,AR] in=0.0
insideOut :: Rate -> UGen -> UGen
insideOut rate in_ = mkUGen Nothing [KR,AR] (Left rate) "InsideOut" [in_] Nothing 1 (Special 0) NoId

-- | instruction synthesis (breakpoint set interpreter)
--
--  Instruction [AR] bufnum=0.0
instruction :: Rate -> UGen -> UGen
instruction rate bufnum = mkUGen Nothing [AR] (Left rate) "Instruction" [bufnum] Nothing 1 (Special 0) NoId

-- | Raw version of the JPverb algorithmic reverberator, designed to produce long tails with chorusing
--
--  JPverbRaw [KR,AR] in1=0.0 in2=0.0 damp=0.0 earlydiff=0.707 highband=2000.0 highx=1.0 lowband=500.0 lowx=1.0 mdepth=0.1 mfreq=2.0 midx=1.0 size=1.0 t60=1.0;    FILTER: TRUE
jPverbRaw :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
jPverbRaw in1 in2 damp earlydiff highband highx lowband lowx mdepth mfreq midx size t60 = mkUGen Nothing [KR,AR] (Right [0]) "JPverbRaw" [in1,in2,damp,earlydiff,highband,highx,lowband,lowx,mdepth,mfreq,midx,size,t60] Nothing 2 (Special 0) NoId

-- | k-means classification in real time
--
--  KMeansRT [KR] bufnum=0.0 inputdata=0.0 k=5.0 gate=1.0 reset=0.0 learn=1.0
kMeansRT :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
kMeansRT rate bufnum inputdata k gate_ reset learn = mkUGen Nothing [KR] (Left rate) "KMeansRT" [bufnum,inputdata,k,gate_,reset,learn] Nothing 1 (Special 0) NoId

-- | Running score of maximum correlation of chromagram with key profiles
--
--  KeyClarity [KR] chain=0.0 keydecay=2.0 chromaleak=0.5
keyClarity :: Rate -> UGen -> UGen -> UGen -> UGen
keyClarity rate chain keydecay chromaleak = mkUGen Nothing [KR] (Left rate) "KeyClarity" [chain,keydecay,chromaleak] Nothing 1 (Special 0) NoId

-- | Find best correlated key mode with chromagram between major, minor and chromatic cluster
--
--  KeyMode [KR] chain=0.0 keydecay=2.0 chromaleak=0.5
keyMode :: Rate -> UGen -> UGen -> UGen -> UGen
keyMode rate chain keydecay chromaleak = mkUGen Nothing [KR] (Left rate) "KeyMode" [chain,keydecay,chromaleak] Nothing 1 (Special 0) NoId

-- | K-means Oscillator
--
--  KmeansToBPSet1 [AR] freq=440.0 numdatapoints=20.0 maxnummeans=4.0 nummeans=4.0 tnewdata=1.0 tnewmeans=1.0 soft=1.0 bufnum=0.0
kmeansToBPSet1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
kmeansToBPSet1 rate freq numdatapoints maxnummeans nummeans tnewdata tnewmeans soft bufnum = mkUGen Nothing [AR] (Left rate) "KmeansToBPSet1" [freq,numdatapoints,maxnummeans,nummeans,tnewdata,tnewmeans,soft,bufnum] Nothing 1 (Special 0) NoId

-- | random walk step
--
--  LFBrownNoise0 [KR,AR] freq=20.0 dev=1.0 dist=0.0;    NONDET
lfBrownNoise0 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise0 z rate freq dev dist = mkUGen Nothing [KR,AR] (Left rate) "LFBrownNoise0" [freq,dev,dist] Nothing 1 (Special 0) (toUId z)

-- | random walk linear interp
--
--  LFBrownNoise1 [KR,AR] freq=20.0 dev=1.0 dist=0.0;    NONDET
lfBrownNoise1 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise1 z rate freq dev dist = mkUGen Nothing [KR,AR] (Left rate) "LFBrownNoise1" [freq,dev,dist] Nothing 1 (Special 0) (toUId z)

-- | random walk cubic interp
--
--  LFBrownNoise2 [KR,AR] freq=20.0 dev=1.0 dist=0.0;    NONDET
lfBrownNoise2 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise2 z rate freq dev dist = mkUGen Nothing [KR,AR] (Left rate) "LFBrownNoise2" [freq,dev,dist] Nothing 1 (Special 0) (toUId z)

-- | Live Linear Predictive Coding Analysis and Resynthesis
--
--  LPCAnalyzer [AR] input=0.0 source=1.0e-2 n=256.0 p=10.0 testE=0.0 delta=0.999 windowtype=0.0;    FILTER: TRUE
lpcAnalyzer :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lpcAnalyzer input source n p testE delta windowtype = mkUGen Nothing [AR] (Right [0,1]) "LPCAnalyzer" [input,source,n,p,testE,delta,windowtype] Nothing 1 (Special 0) NoId

-- | Linear Predictive Coding Gone Wrong
--
--  LPCError [AR] input=0.0 p=10.0
lpcError :: Rate -> UGen -> UGen -> UGen
lpcError rate input p = mkUGen Nothing [AR] (Left rate) "LPCError" [input,p] Nothing 1 (Special 0) NoId

-- | Utilize LPC data
--
--  LPCSynth [AR] buffer=0.0 signal=0.0 pointer=0.0
lpcSynth :: UGen -> UGen -> UGen -> UGen
lpcSynth buffer signal pointer = mkUGen Nothing [AR] (Left AR) "LPCSynth" [buffer,signal,pointer] Nothing 1 (Special 0) NoId

-- | Utilize LPC data
--
--  LPCVals [KR,AR] buffer=0.0 pointer=0.0
lpcVals :: UGen -> UGen -> UGen
lpcVals buffer pointer = mkUGen Nothing [KR,AR] (Left AR) "LPCVals" [buffer,pointer] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  LPF1 [KR,AR] in=0.0 freq=1000.0
lpf1 :: Rate -> UGen -> UGen -> UGen
lpf1 rate in_ freq = mkUGen Nothing [KR,AR] (Left rate) "LPF1" [in_,freq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LPF18 [AR] in=0.0 freq=100.0 res=1.0 dist=0.4
lpf18 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
lpf18 rate in_ freq res dist = mkUGen Nothing [AR] (Left rate) "LPF18" [in_,freq,res,dist] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LPFVS6 [KR,AR] in=0.0 freq=1000.0 slope=0.5
lpfvs6 :: Rate -> UGen -> UGen -> UGen -> UGen
lpfvs6 rate in_ freq slope_ = mkUGen Nothing [KR,AR] (Left rate) "LPFVS6" [in_,freq,slope_] Nothing 1 (Special 0) NoId

-- | A digital model of the Buchla Lowpass-Gate
--
--  LPG [AR] input=0.0 controlinput=0.0 controloffset=0.0 controlscale=1.0 vca=1.0 resonance=1.5 lowpassmode=1.0 linearity=1.0
lpg :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lpg input controlinput controloffset controlscale vca resonance lowpassmode linearity = mkUGen Nothing [AR] (Right [0]) "LPG" [input,controlinput,controloffset,controlscale,vca,resonance,lowpassmode,linearity] Nothing 1 (Special 0) NoId

-- | Linear Time Invariant General Filter Equation
--
--  LTI [AR] input=0.0 bufnuma=0.0 bufnumb=1.0
lti :: Rate -> UGen -> UGen -> UGen -> UGen
lti rate input bufnuma bufnumb = mkUGen Nothing [AR] (Left rate) "LTI" [input,bufnuma,bufnumb] Nothing 1 (Special 0) NoId

-- | latoocarfian 2D chaotic generator
--
--  Latoocarfian2DC [KR,AR] minfreq=11025.0 maxfreq=22050.0 a=1.0 b=3.0 c=0.5 d=0.5 x0=0.34082301375036 y0=-0.38270086971332
latoocarfian2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfian2DC rate minfreq maxfreq a b c d x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Latoocarfian2DC" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | latoocarfian 2D chaotic generator
--
--  Latoocarfian2DL [KR,AR] minfreq=11025.0 maxfreq=22050.0 a=1.0 b=3.0 c=0.5 d=0.5 x0=0.34082301375036 y0=-0.38270086971332
latoocarfian2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfian2DL rate minfreq maxfreq a b c d x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Latoocarfian2DL" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | latoocarfian 2D chaotic generator
--
--  Latoocarfian2DN [KR,AR] minfreq=11025.0 maxfreq=22050.0 a=1.0 b=3.0 c=0.5 d=0.5 x0=0.34082301375036 y0=-0.38270086971332
latoocarfian2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfian2DN rate minfreq maxfreq a b c d x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Latoocarfian2DN" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LatoocarfianTrig [KR,AR] minfreq=5.0 maxfreq=10.0 a=1.0 b=3.0 c=0.5 d=0.5 x0=0.34082301375036 y0=-0.38270086971332
latoocarfianTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianTrig rate minfreq maxfreq a b c d x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "LatoocarfianTrig" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | Emit a sequence of triggers at specified time offsets
--
--  ListTrig [KR] bufnum=0.0 reset=0.0 offset=0.0 numframes=0.0
listTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
listTrig rate bufnum reset offset numframes = mkUGen Nothing [KR] (Left rate) "ListTrig" [bufnum,reset,offset,numframes] Nothing 1 (Special 0) NoId

-- | Emit a sequence of triggers at specified time offsets
--
--  ListTrig2 [KR] bufnum=0.0 reset=0.0 numframes=0.0
listTrig2 :: Rate -> UGen -> UGen -> UGen -> UGen
listTrig2 rate bufnum reset numframes = mkUGen Nothing [KR] (Left rate) "ListTrig2" [bufnum,reset,numframes] Nothing 1 (Special 0) NoId

-- | Store values to a buffer, whenever triggered
--
--  Logger [KR] inputArray=0.0 trig=0.0 bufnum=0.0 reset=0.0
logger :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
logger rate inputArray trig_ bufnum reset = mkUGen Nothing [KR] (Left rate) "Logger" [inputArray,trig_,bufnum,reset] Nothing 1 (Special 0) NoId

-- | sample looping oscillator
--
--  LoopBuf [AR] bufnum=0.0 rate=1.0 gate=1.0 startPos=0.0 startLoop=0.0 endLoop=0.0 interpolation=2.0;    NC INPUT: True
loopBuf :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
loopBuf numChannels rate bufnum rate_ gate_ startPos startLoop endLoop interpolation = mkUGen Nothing [AR] (Left rate) "LoopBuf" [bufnum,rate_,gate_,startPos,startLoop,endLoop,interpolation] Nothing numChannels (Special 0) NoId

-- | lorenz 2D chaotic generator
--
--  Lorenz2DC [KR,AR] minfreq=11025.0 maxfreq=22050.0 s=10.0 r=28.0 b=2.6666667 h=2.0e-2 x0=9.0879182417163e-2 y0=2.97077458055 z0=24.282041054363
lorenz2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenz2DC rate minfreq maxfreq s r b h x0 y0 z0 = mkUGen Nothing [KR,AR] (Left rate) "Lorenz2DC" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | lorenz 2D chaotic generator
--
--  Lorenz2DL [KR,AR] minfreq=11025.0 maxfreq=22050.0 s=10.0 r=28.0 b=2.6666667 h=2.0e-2 x0=9.0879182417163e-2 y0=2.97077458055 z0=24.282041054363
lorenz2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenz2DL rate minfreq maxfreq s r b h x0 y0 z0 = mkUGen Nothing [KR,AR] (Left rate) "Lorenz2DL" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | lorenz 2D chaotic generator
--
--  Lorenz2DN [KR,AR] minfreq=11025.0 maxfreq=22050.0 s=10.0 r=28.0 b=2.6666667 h=2.0e-2 x0=9.0879182417163e-2 y0=2.97077458055 z0=24.282041054363
lorenz2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenz2DN rate minfreq maxfreq s r b h x0 y0 z0 = mkUGen Nothing [KR,AR] (Left rate) "Lorenz2DN" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | lorenz chaotic trigger generator
--
--  LorenzTrig [KR,AR] minfreq=11025.0 maxfreq=22050.0 s=10.0 r=28.0 b=2.6666667 h=2.0e-2 x0=9.0879182417163e-2 y0=2.97077458055 z0=24.282041054363
lorenzTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenzTrig rate minfreq maxfreq s r b h x0 y0 z0 = mkUGen Nothing [KR,AR] (Left rate) "LorenzTrig" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | simple resonating lowpass filter
--
--  Lores [AR] in=0.0 freq=880.0 res=0.5
lores :: UGen -> UGen -> UGen -> UGen
lores in_ freq res = mkUGen Nothing [AR] (Right [0]) "Lores" [in_,freq,res] Nothing 1 (Special 0) NoId

-- | 2-species Predator-Prey model
--
--  LotkaVolterra [AR] freq=22050.0 a=1.5 b=1.5 c=0.5 d=1.5 h=5.0e-2 xi=1.0 yi=0.2
lotkaVolterra :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lotkaVolterra rate freq a b c d h xi yi = mkUGen Nothing [AR] (Left rate) "LotkaVolterra" [freq,a,b,c,d,h,xi,yi] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  MCLDChaosGen [] maxSize=0.0
mcldChaosGen :: Rate -> UGen -> UGen
mcldChaosGen rate maxSize = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "MCLDChaosGen" [maxSize] Nothing 1 (Special 0) NoId

-- | First order Markov Chain implementation for audio signals
--
--  MarkovSynth [AR] in=0.0 isRecording=1.0 waitTime=2.0 tableSize=10.0
markovSynth :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
markovSynth rate in_ isRecording waitTime tableSize = mkUGen Nothing [AR] (Left rate) "MarkovSynth" [in_,isRecording,waitTime,tableSize] Nothing 1 (Special 0) NoId

-- | Real time sparse representation
--
--  MatchingP [KR,AR] dict=0.0 in=0.0 dictsize=1.0 ntofind=1.0 hop=1.0 method=0.0
matchingP :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
matchingP rate dict in_ dictsize ntofind hop method = mkUGen Nothing [KR,AR] (Left rate) "MatchingP" [dict,in_,dictsize,ntofind,hop,method] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  MatchingPResynth [KR,AR] dict=0.0 method=0.0 trigger=0.0 residual=0.0 activs=0.0
matchingPResynth :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
matchingPResynth rate dict method trigger residual activs = mkUGen Nothing [KR,AR] (Left rate) "MatchingPResynth" [dict,method,trigger,residual,activs] Nothing 1 (Special 0) NoId

-- | maximum within last x samples
--
--  Max [KR] in=0.0 numsamp=64.0
max :: Rate -> UGen -> UGen -> UGen
max rate in_ numsamp = mkUGen Nothing [KR] (Left rate) "Max" [in_,numsamp] Nothing 1 (Special 0) NoId

-- | Tracks and prints amplitudes
--
--  Maxamp [AR] in=0.0 numSamps=1000.0
maxamp :: Rate -> UGen -> UGen -> UGen
maxamp rate in_ numSamps = mkUGen Nothing [AR] (Left rate) "Maxamp" [in_,numSamps] Nothing 1 (Special 0) NoId

-- | Piano synthesiser
--
--  MdaPiano [AR] freq=440.0 gate=1.0 vel=100.0 decay=0.8 release=0.8 hard=0.8 velhard=0.8 muffle=0.8 velmuff=0.8 velcurve=0.8 stereo=0.2 tune=0.5 random=0.1 stretch=0.1 sustain=0.0
mdaPiano :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
mdaPiano rate freq gate_ vel decay_ release hard velhard muffle velmuff velcurve stereo tune random stretch sustain = mkUGen Nothing [AR] (Left rate) "MdaPiano" [freq,gate_,vel,decay_,release,hard,velhard,muffle,velmuff,velcurve,stereo,tune,random,stretch,sustain] Nothing 2 (Special 0) NoId

-- | Mean of recent values, triggered
--
--  MeanTriggered [KR,AR] in=0.0 trig=0.0 length=10.0
meanTriggered :: Rate -> UGen -> UGen -> UGen -> UGen
meanTriggered rate in_ trig_ length_ = mkUGen Nothing [KR,AR] (Left rate) "MeanTriggered" [in_,trig_,length_] Nothing 1 (Special 0) NoId

-- | Meddis cochlear hair cell model
--
--  Meddis [KR,AR] input=0.0;    FILTER: TRUE
meddis :: UGen -> UGen
meddis input = mkUGen Nothing [KR,AR] (Right [0]) "Meddis" [input] Nothing 1 (Special 0) NoId

-- | Separate harmonic and percussive parts of a signal
--
--  MedianSeparation [] fft=0.0 fftharmonic=0.0 fftpercussive=0.0 fftsize=1024.0 mediansize=17.0 hardorsoft=0.0 p=2.0 medianormax=0.0
medianSeparation :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
medianSeparation rate fft_ fftharmonic fftpercussive fftsize mediansize hardorsoft p medianormax = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "MedianSeparation" [fft_,fftharmonic,fftpercussive,fftsize,mediansize,hardorsoft,p,medianormax] Nothing 2 (Special 0) NoId

-- | Median of recent values, triggered
--
--  MedianTriggered [KR,AR] in=0.0 trig=0.0 length=10.0
medianTriggered :: Rate -> UGen -> UGen -> UGen -> UGen
medianTriggered rate in_ trig_ length_ = mkUGen Nothing [KR,AR] (Left rate) "MedianTriggered" [in_,trig_,length_] Nothing 1 (Special 0) NoId

-- | Waveguide mesh physical models of drum membranes
--
--  MembraneCircle [AR] excitation=0.0 tension=5.0e-2 loss=0.99999
membraneCircle :: Rate -> UGen -> UGen -> UGen -> UGen
membraneCircle rate excitation tension loss = mkUGen Nothing [AR] (Left rate) "MembraneCircle" [excitation,tension,loss] Nothing 1 (Special 0) NoId

-- | Waveguide mesh physical models of drum membranes
--
--  MembraneHexagon [AR] excitation=0.0 tension=5.0e-2 loss=0.99999
membraneHexagon :: Rate -> UGen -> UGen -> UGen -> UGen
membraneHexagon rate excitation tension loss = mkUGen Nothing [AR] (Left rate) "MembraneHexagon" [excitation,tension,loss] Nothing 1 (Special 0) NoId

-- | Metronome
--
--  Metro [KR,AR] bpm=0.0 numBeats=0.0
metro :: Rate -> UGen -> UGen -> UGen
metro rate bpm numBeats = mkUGen Nothing [KR,AR] (Left rate) "Metro" [bpm,numBeats] Nothing 1 (Special 0) NoId

-- | a macro oscillator
--
--  MiBraids [AR] pitch=60.0 timbre=0.5 color=0.5 model=0.0 trig=0.0 resamp=0.0 decim=0.0 bits=0.0 ws=0.0
miBraids :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miBraids rate pitch_ timbre color model trig_ resamp decim bits ws = mkUGen Nothing [AR] (Left rate) "MiBraids" [pitch_,timbre,color,model,trig_,resamp,decim,bits,ws] Nothing 1 (Special 0) NoId

-- | granular audio processor and texture synthesizer
--
--  MiClouds [AR] pit=0.0 pos=0.5 size=0.25 dens=0.4 tex=0.5 drywet=0.5 in_gain=1.0 spread=0.5 rvb=0.0 fb=0.0 freeze=0.0 mode=0.0 lofi=0.0 trig=0.0 *inputArray=0.0;    MCE=1, REORDERS INPUTS: [14,0,1,2,3,4,5,6,7,8,9,10,11,12,13]
miClouds :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miClouds rate pit pos size dens tex drywet in_gain spread rvb fb freeze mode lofi trig_ inputArray = mkUGen Nothing [AR] (Left rate) "MiClouds" [pit,pos,size,dens,tex,drywet,in_gain,spread,rvb,fb,freeze,mode,lofi,trig_] (Just [inputArray]) 2 (Special 0) NoId

-- | Physical modelling based on Modal Synthesis.
--
--  MiElements [AR] blow_in=0.0 strike_in=0.0 gate=0.0 pit=48.0 strength=0.5 contour=0.2 bow_level=0.0 blow_level=0.0 strike_level=0.0 flow=0.5 mallet=0.5 bow_timb=0.5 blow_timb=0.5 strike_timb=0.5 geom=0.25 bright=0.5 damp=0.7 pos=0.2 space=0.3 model=0.0 easteregg=0.0
miElements :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miElements rate blow_in strike_in gate_ pit strength contour bow_level blow_level strike_level flow mallet bow_timb blow_timb strike_timb geom bright damp pos space model easteregg = mkUGen Nothing [AR] (Left rate) "MiElements" [blow_in,strike_in,gate_,pit,strength,contour,bow_level,blow_level,strike_level,flow,mallet,bow_timb,blow_timb,strike_timb,geom,bright,damp,pos,space,model,easteregg] Nothing 2 (Special 0) NoId

-- | µ-law audio companding
--
--  MiMu [AR] in=0.0 gain=1.0 bypass=0.0
miMu :: Rate -> UGen -> UGen -> UGen -> UGen
miMu rate in_ gain bypass = mkUGen Nothing [AR] (Left rate) "MiMu" [in_,gain,bypass] Nothing 1 (Special 0) NoId

-- | FM Synth-Voice based on 'ominous'
--
--  MiOmi [AR] audio_in=0.0 gate=0.0 pit=48.0 contour=0.2 detune=0.25 level1=0.5 level2=0.5 ratio1=0.5 ratio2=0.5 fm1=0.0 fm2=0.0 fb=0.0 xfb=0.0 filter_mode=0.0 cutoff=0.5 reson=0.0 strength=0.5 env=0.5 rotate=0.2 space=0.5
miOmi :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miOmi rate audio_in gate_ pit contour detune level1 level2 ratio1 ratio2 fm1 fm2 fb xfb filter_mode cutoff reson strength env rotate_ space = mkUGen Nothing [AR] (Left rate) "MiOmi" [audio_in,gate_,pit,contour,detune,level1,level2,ratio1,ratio2,fm1,fm2,fb,xfb,filter_mode,cutoff,reson,strength,env,rotate_,space] Nothing 2 (Special 0) NoId

-- | a macro oscillator
--
--  MiPlaits [AR] pitch=60.0 engine=0.0 harm=0.1 timbre=0.5 morph=0.5 trigger=0.0 level=0.0 fm_mod=0.0 timb_mod=0.0 morph_mod=0.0 decay=0.5 lpg_colour=0.5
miPlaits :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miPlaits rate pitch_ engine harm timbre morph trigger level fm_mod timb_mod morph_mod decay_ lpg_colour = mkUGen Nothing [AR] (Left rate) "MiPlaits" [pitch_,engine,harm,timbre,morph,trigger,level,fm_mod,timb_mod,morph_mod,decay_,lpg_colour] Nothing 2 (Special 0) NoId

-- | a resonator
--
--  MiRings [AR] in=0.0 trig=0.0 pit=60.0 struct=0.25 bright=0.5 damp=0.7 pos=0.25 model=0.0 poly=1.0 intern_exciter=0.0 easteregg=0.0 bypass=0.0
miRings :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miRings rate in_ trig_ pit struct bright damp pos model poly intern_exciter easteregg bypass = mkUGen Nothing [AR] (Left rate) "MiRings" [in_,trig_,pit,struct,bright,damp,pos,model,poly,intern_exciter,easteregg,bypass] Nothing 2 (Special 0) NoId

-- | Classic resonant LP filter
--
--  MiRipples [AR] in=0.0 cf=0.3 reson=0.2 drive=1.0;    FILTER: TRUE
miRipples :: UGen -> UGen -> UGen -> UGen -> UGen
miRipples in_ cf reson drive = mkUGen Nothing [AR] (Right [0]) "MiRipples" [in_,cf,reson,drive] Nothing 1 (Special 0) NoId

-- | a quad LFO
--
--  MiTides [AR] freq=1.0 shape=0.5 slope=0.5 smooth=0.5 shift=0.2 trig=0.0 clock=0.0 output_mode=3.0 ramp_mode=1.0 ratio=9.0 rate=1.0
miTides :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miTides rate freq shape slope_ smooth shift trig_ clock output_mode ramp_mode ratio rate_ = mkUGen Nothing [AR] (Left rate) "MiTides" [freq,shape,slope_,smooth,shift,trig_,clock,output_mode,ramp_mode,ratio,rate_] Nothing 4 (Special 0) NoId

-- | stereo reverb
--
--  MiVerb [AR] time=0.7 drywet=0.5 damp=0.5 hp=5.0e-2 freeze=0.0 diff=0.625 *inputArray=0.0;    MCE=1, FILTER: TRUE, REORDERS INPUTS: [6,0,1,2,3,4,5]
miVerb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miVerb time drywet damp hp freeze diff inputArray = mkUGen Nothing [AR] (Right [6]) "MiVerb" [time,drywet,damp,hp,freeze,diff] (Just [inputArray]) 2 (Special 0) NoId

-- | (Undocumented class)
--
--  MiWarps [AR] carrier=0.0 modulator=0.0 lev1=0.5 lev2=0.5 algo=0.0 timb=0.0 osc=1.0 pit=60.0 easteregg=0.0
miWarps :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miWarps rate carrier modulator lev1 lev2 algo timb osc_ pit easteregg = mkUGen Nothing [AR] (Left rate) "MiWarps" [carrier,modulator,lev1,lev2,algo,timb,osc_,pit,easteregg] Nothing 2 (Special 0) NoId

-- | Granulates real-time input
--
--  MonoGrain [AR] in=0.0 winsize=0.1 grainrate=10.0 winrandpct=0.0
monoGrain :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
monoGrain rate in_ winsize grainrate winrandpct = mkUGen Nothing [AR] (Left rate) "MonoGrain" [in_,winsize,grainrate,winrandpct] Nothing 1 (Special 0) NoId

-- | Granulates real-time input with Ambisonic panning
--
--  MonoGrainBF [AR] in=0.0 winsize=0.1 grainrate=10.0 winrandpct=0.0 azimuth=0.0 azrand=0.0 elevation=0.0 elrand=0.0 rho=1.0
monoGrainBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
monoGrainBF rate in_ winsize grainrate winrandpct azimuth azrand elevation elrand rho = mkUGen Nothing [AR] (Left rate) "MonoGrainBF" [in_,winsize,grainrate,winrandpct,azimuth,azrand,elevation,elrand,rho] Nothing 4 (Special 0) NoId

-- | Moog Filter Emulation
--
--  MoogLadder [KR,AR] in=0.0 ffreq=440.0 res=0.0;    FILTER: TRUE
moogLadder :: UGen -> UGen -> UGen -> UGen
moogLadder in_ ffreq res = mkUGen Nothing [KR,AR] (Right [0]) "MoogLadder" [in_,ffreq,res] Nothing 1 (Special 0) NoId

-- | Moog  filter emulation
--
--  MoogVCF [AR] in=0.0 fco=0.0 res=0.0
moogVCF :: Rate -> UGen -> UGen -> UGen -> UGen
moogVCF rate in_ fco res = mkUGen Nothing [AR] (Left rate) "MoogVCF" [in_,fco,res] Nothing 1 (Special 0) NoId

-- | Non Linear Filter Equation
--
--  NL [AR] input=0.0 bufnuma=0.0 bufnumb=1.0 guard1=1000.0 guard2=100.0
nl :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nl rate input bufnuma bufnumb guard1 guard2 = mkUGen Nothing [AR] (Left rate) "NL" [input,bufnuma,bufnumb,guard1,guard2] Nothing 1 (Special 0) NoId

-- | Arbitrary Non Linear Filter Equation
--
--  NL2 [AR] input=0.0 bufnum=0.0 maxsizea=10.0 maxsizeb=10.0 guard1=1000.0 guard2=100.0
nl2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nl2 rate input bufnum maxsizea maxsizeb guard1 guard2 = mkUGen Nothing [AR] (Left rate) "NL2" [input,bufnum,maxsizea,maxsizeb,guard1,guard2] Nothing 1 (Special 0) NoId

-- | Non-linear Filter
--
--  NLFiltC [KR,AR] input=0.0 a=0.0 b=0.0 d=0.0 c=0.0 l=0.0
nlFiltC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nlFiltC rate input a b d c l = mkUGen Nothing [KR,AR] (Left rate) "NLFiltC" [input,a,b,d,c,l] Nothing 1 (Special 0) NoId

-- | Non-linear Filter
--
--  NLFiltL [KR,AR] input=0.0 a=0.0 b=0.0 d=0.0 c=0.0 l=0.0
nlFiltL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nlFiltL rate input a b d c l = mkUGen Nothing [KR,AR] (Left rate) "NLFiltL" [input,a,b,d,c,l] Nothing 1 (Special 0) NoId

-- | Non-linear Filter
--
--  NLFiltN [KR,AR] input=0.0 a=0.0 b=0.0 d=0.0 c=0.0 l=0.0
nlFiltN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nlFiltN rate input a b d c l = mkUGen Nothing [KR,AR] (Left rate) "NLFiltN" [input,a,b,d,c,l] Nothing 1 (Special 0) NoId

-- | physical modeling simulation; N tubes
--
--  NTube [AR] input=0.0 lossarray=1.0 karray=0.0 delaylengtharray=0.0
nTube :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
nTube rate input lossarray karray delaylengtharray = mkUGen Nothing [AR] (Left rate) "NTube" [input,lossarray,karray,delaylengtharray] Nothing 1 (Special 0) NoId

-- | Find the nearest-neighbours in a set of points
--
--  NearestN [KR] treebuf=0.0 in=0.0 gate=1.0 num=1.0
nearestN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
nearestN rate treebuf in_ gate_ num = mkUGen Nothing [KR] (Left rate) "NearestN" [treebuf,in_,gate_,num] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  NeedleRect [AR] rate=1.0 imgWidth=100.0 imgHeight=100.0 rectX=0.0 rectY=0.0 rectW=100.0 rectH=100.0
needleRect :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
needleRect rate rate_ imgWidth imgHeight rectX rectY rectW rectH = mkUGen Nothing [AR] (Left rate) "NeedleRect" [rate_,imgWidth,imgHeight,rectX,rectY,rectW,rectH] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  NestedAllpassC [AR] in=0.0 maxdelay1=3.6e-2 delay1=3.6e-2 gain1=8.0e-2 maxdelay2=3.0e-2 delay2=3.0e-2 gain2=0.3;    FILTER: TRUE
nestedAllpassC :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nestedAllpassC in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 = mkUGen Nothing [AR] (Right [0]) "NestedAllpassC" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  NestedAllpassL [AR] in=0.0 maxdelay1=3.6e-2 delay1=3.6e-2 gain1=8.0e-2 maxdelay2=3.0e-2 delay2=3.0e-2 gain2=0.3;    FILTER: TRUE
nestedAllpassL :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nestedAllpassL in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 = mkUGen Nothing [AR] (Right [0]) "NestedAllpassL" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  NestedAllpassN [AR] in=0.0 maxdelay1=3.6e-2 delay1=3.6e-2 gain1=8.0e-2 maxdelay2=3.0e-2 delay2=3.0e-2 gain2=0.3;    FILTER: TRUE
nestedAllpassN :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nestedAllpassN in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 = mkUGen Nothing [AR] (Right [0]) "NestedAllpassN" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSFold4 [AR] in=0.0 lo=0.0 hi=0.0
osFold4 :: Rate -> UGen -> UGen -> UGen -> UGen
osFold4 rate in_ lo hi = mkUGen Nothing [AR] (Left rate) "OSFold4" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSFold8 [AR] in=0.0 lo=0.0 hi=0.0
osFold8 :: Rate -> UGen -> UGen -> UGen -> UGen
osFold8 rate in_ lo hi = mkUGen Nothing [AR] (Left rate) "OSFold8" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSTrunc4 [AR] in=0.0 quant=0.5
osTrunc4 :: Rate -> UGen -> UGen -> UGen
osTrunc4 rate in_ quant = mkUGen Nothing [AR] (Left rate) "OSTrunc4" [in_,quant] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSTrunc8 [AR] in=0.0 quant=0.5
osTrunc8 :: Rate -> UGen -> UGen -> UGen
osTrunc8 rate in_ quant = mkUGen Nothing [AR] (Left rate) "OSTrunc8" [in_,quant] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSWrap4 [AR] in=0.0 lo=0.0 hi=0.0
osWrap4 :: Rate -> UGen -> UGen -> UGen -> UGen
osWrap4 rate in_ lo hi = mkUGen Nothing [AR] (Left rate) "OSWrap4" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSWrap8 [AR] in=0.0 lo=0.0 hi=0.0
osWrap8 :: Rate -> UGen -> UGen -> UGen -> UGen
osWrap8 rate in_ lo hi = mkUGen Nothing [AR] (Left rate) "OSWrap8" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Extract basic statistics from a series of onset triggers
--
--  OnsetStatistics [KR] input=0.0 windowsize=1.0 hopsize=0.1
onsetStatistics :: Rate -> UGen -> UGen -> UGen -> UGen
onsetStatistics rate input windowsize hopsize = mkUGen Nothing [KR] (Left rate) "OnsetStatistics" [input,windowsize,hopsize] Nothing 3 (Special 0) NoId

-- | Chemical reaction modelling Oscillator
--
--  Oregonator [AR] reset=0.0 rate=1.0e-2 epsilon=1.0 mu=1.0 q=1.0 initx=0.5 inity=0.5 initz=0.5
oregonator :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
oregonator rate reset rate_ epsilon mu q initx inity initz = mkUGen Nothing [AR] (Left rate) "Oregonator" [reset,rate_,epsilon,mu,q,initx,inity,initz] Nothing 3 (Special 0) NoId

-- | Piano physical model.
--
--  OteyPiano [AR] freq=440.0 vel=1.0 t_gate=0.0 rmin=0.35 rmax=2.0 rampl=4.0 rampr=8.0 rcore=1.0 lmin=7.0e-2 lmax=1.4 lampl=-4.0 lampr=4.0 rho=1.0 e=1.0 zb=1.0 zh=0.0 mh=1.0 k=0.2 alpha=1.0 p=1.0 hpos=0.142 loss=1.0 detune=3.0e-4 hammer_type=1.0
oteyPiano :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
oteyPiano rate freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type = mkUGen Nothing [AR] (Left rate) "OteyPiano" [freq,vel,t_gate,rmin,rmax,rampl,rampr,rcore,lmin,lmax,lampl,lampr,rho,e,zb,zh,mh,k,alpha,p,hpos,loss,detune,hammer_type] Nothing 1 (Special 0) NoId

-- | Piano physical model.
--
--  OteyPianoStrings [AR] freq=440.0 vel=1.0 t_gate=0.0 rmin=0.35 rmax=2.0 rampl=4.0 rampr=8.0 rcore=1.0 lmin=7.0e-2 lmax=1.4 lampl=-4.0 lampr=4.0 rho=1.0 e=1.0 zb=1.0 zh=0.0 mh=1.0 k=0.2 alpha=1.0 p=1.0 hpos=0.142 loss=1.0 detune=3.0e-4 hammer_type=1.0
oteyPianoStrings :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
oteyPianoStrings rate freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type = mkUGen Nothing [AR] (Left rate) "OteyPianoStrings" [freq,vel,t_gate,rmin,rmax,rampl,rampr,rcore,lmin,lmax,lampl,lampr,rho,e,zb,zh,mh,k,alpha,p,hpos,loss,detune,hammer_type] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OteySoundBoard [AR] inp=0.0 c1=20.0 c3=20.0 mix=0.8
oteySoundBoard :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
oteySoundBoard rate inp c1 c3 mix = mkUGen Nothing [AR] (Left rate) "OteySoundBoard" [inp,c1,c3,mix] Nothing 1 (Special 0) NoId

-- | Return mag and freq data from a CSound pv
--
--  PVInfo [KR,AR] pvbuffer=0.0 binNum=0.0 filePointer=0.0
pvInfo :: Rate -> UGen -> UGen -> UGen -> UGen
pvInfo rate pvbuffer binNum filePointer = mkUGen Nothing [KR,AR] (Left rate) "PVInfo" [pvbuffer,binNum,filePointer] Nothing 2 (Special 0) NoId

-- | Resynthesize Csound PV data
--
--  PVSynth [AR] pvbuffer=0.0 numBins=0.0 binStart=0.0 binSkip=1.0 filePointer=0.0 freqMul=1.0 freqAdd=0.0
pvSynth :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pvSynth rate pvbuffer numBins binStart binSkip filePointer freqMul freqAdd = mkUGen Nothing [AR] (Left rate) "PVSynth" [pvbuffer,numBins,binStart,binSkip,filePointer,freqMul,freqAdd] Nothing 1 (Special 0) NoId

-- | Plays FFT data to a memory buffer
--
--  PV_BinBufRd [KR] buffer=0.0 playbuf=0.0 point=1.0 binStart=0.0 binSkip=1.0 numBins=1.0 clear=0.0
pv_BinBufRd :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinBufRd buffer playbuf_ point binStart binSkip numBins clear = mkUGen Nothing [KR] (Left KR) "PV_BinBufRd" [buffer,playbuf_,point,binStart,binSkip,numBins,clear] Nothing 1 (Special 0) NoId

-- | Delay and Feedback on a bin by bin basis.
--
--  PV_BinDelay [KR] buffer=0.0 maxdelay=0.0 delaybuf=0.0 fbbuf=0.0 hop=0.5
pv_BinDelay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinDelay buffer maxdelay delaybuf fbbuf hop = mkUGen Nothing [KR] (Left KR) "PV_BinDelay" [buffer,maxdelay,delaybuf,fbbuf,hop] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_BinFilter [KR] buffer=0.0 start=0.0 end=0.0
pv_BinFilter :: UGen -> UGen -> UGen -> UGen
pv_BinFilter buffer start end = mkUGen Nothing [KR] (Left KR) "PV_BinFilter" [buffer,start,end] Nothing 1 (Special 0) NoId

-- | Plays FFT data to a memory buffer
--
--  PV_BinPlayBuf [KR] buffer=0.0 playbuf=0.0 rate=1.0 offset=0.0 binStart=0.0 binSkip=1.0 numBins=1.0 loop=0.0 clear=0.0
pv_BinPlayBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinPlayBuf buffer playbuf_ rate_ offset binStart binSkip numBins loop clear = mkUGen Nothing [KR] (Left KR) "PV_BinPlayBuf" [buffer,playbuf_,rate_,offset,binStart,binSkip,numBins,loop,clear] Nothing 1 (Special 0) NoId

-- | Plays FFT data from a memory buffer
--
--  PV_BufRd [KR] buffer=0.0 playbuf=0.0 point=1.0
pv_BufRd :: UGen -> UGen -> UGen -> UGen
pv_BufRd buffer playbuf_ point = mkUGen Nothing [KR] (Left KR) "PV_BufRd" [buffer,playbuf_,point] Nothing 1 (Special 0) NoId

-- | returns common magnitudes
--
--  PV_CommonMag [KR] bufferA=0.0 bufferB=0.0 tolerance=0.0 remove=0.0
pv_CommonMag :: UGen -> UGen -> UGen -> UGen -> UGen
pv_CommonMag bufferA bufferB tolerance remove = mkUGen Nothing [KR] (Left KR) "PV_CommonMag" [bufferA,bufferB,tolerance,remove] Nothing 1 (Special 0) NoId

-- | multiplies common magnitudes
--
--  PV_CommonMul [KR] bufferA=0.0 bufferB=0.0 tolerance=0.0 remove=0.0
pv_CommonMul :: UGen -> UGen -> UGen -> UGen -> UGen
pv_CommonMul bufferA bufferB tolerance remove = mkUGen Nothing [KR] (Left KR) "PV_CommonMul" [bufferA,bufferB,tolerance,remove] Nothing 1 (Special 0) NoId

-- | simple spectral compression/expansion
--
--  PV_Compander [KR] buffer=0.0 thresh=50.0 slopeBelow=1.0 slopeAbove=1.0
pv_Compander :: UGen -> UGen -> UGen -> UGen -> UGen
pv_Compander buffer thresh slopeBelow slopeAbove = mkUGen Nothing [KR] (Left KR) "PV_Compander" [buffer,thresh,slopeBelow,slopeAbove] Nothing 1 (Special 0) NoId

-- | zero bins with interpolation
--
--  PV_Cutoff [KR] bufferA=0.0 bufferB=0.0 wipe=0.0
pv_Cutoff :: UGen -> UGen -> UGen -> UGen
pv_Cutoff bufferA bufferB wipe = mkUGen Nothing [KR] (Left KR) "PV_Cutoff" [bufferA,bufferB,wipe] Nothing 1 (Special 0) NoId

-- | Return the even numbered bins in an FFT buffer
--
--  PV_EvenBin [KR] buffer=0.0
pv_EvenBin :: UGen -> UGen
pv_EvenBin buffer = mkUGen Nothing [KR] (Left KR) "PV_EvenBin" [buffer] Nothing 1 (Special 0) NoId

-- | extract a repeating loop out from audio
--
--  PV_ExtractRepeat [KR] buffer=0.0 loopbuf=0.0 loopdur=0.0 memorytime=30.0 which=0.0 ffthop=0.5 thresh=1.0
pv_ExtractRepeat :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_ExtractRepeat buffer loopbuf_ loopdur memorytime which ffthop thresh = mkUGen Nothing [KR] (Left KR) "PV_ExtractRepeat" [buffer,loopbuf_,loopdur,memorytime,which,ffthop,thresh] Nothing 1 (Special 0) NoId

-- | Freeze FFT frames
--
--  PV_Freeze [KR] buffer=0.0 freeze=0.0
pv_Freeze :: UGen -> UGen -> UGen
pv_Freeze buffer freeze = mkUGen Nothing [KR] (Left KR) "PV_Freeze" [buffer,freeze] Nothing 1 (Special 0) NoId

-- | Store FFT data in another buffer for other use
--
--  PV_FreqBuffer [KR] buffer=0.0 databuffer=0.0
pv_FreqBuffer :: UGen -> UGen -> UGen
pv_FreqBuffer buffer databuffer = mkUGen Nothing [KR] (Left KR) "PV_FreqBuffer" [buffer,databuffer] Nothing 1 (Special 0) NoId

-- | Invert FFT frames
--
--  PV_Invert [KR] buffer=0.0
pv_Invert :: UGen -> UGen
pv_Invert buffer = mkUGen Nothing [KR] (Left KR) "PV_Invert" [buffer] Nothing 1 (Special 0) NoId

-- | Store FFT data in another buffer for other use
--
--  PV_MagBuffer [KR] buffer=0.0 databuffer=0.0
pv_MagBuffer :: UGen -> UGen -> UGen
pv_MagBuffer buffer databuffer = mkUGen Nothing [KR] (Left KR) "PV_MagBuffer" [buffer,databuffer] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagExp [KR] buffer=0.0
pv_MagExp :: UGen -> UGen
pv_MagExp buffer = mkUGen Nothing [KR] (Left KR) "PV_MagExp" [buffer] Nothing 1 (Special 0) NoId

-- | reduces magnitudes above or below thresh
--
--  PV_MagGate [KR] buffer=0.0 thresh=1.0 remove=0.0
pv_MagGate :: UGen -> UGen -> UGen -> UGen
pv_MagGate buffer thresh remove = mkUGen Nothing [KR] (Left KR) "PV_MagGate" [buffer,thresh,remove] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagLog [KR] buffer=0.0
pv_MagLog :: UGen -> UGen
pv_MagLog buffer = mkUGen Nothing [KR] (Left KR) "PV_MagLog" [buffer] Nothing 1 (Special 0) NoId

-- | Remap magnitudes to a new mag curve
--
--  PV_MagMap [KR] buffer=0.0 mapbuf=0.0
pv_MagMap :: UGen -> UGen -> UGen
pv_MagMap buffer mapbuf = mkUGen Nothing [KR] (Left KR) "PV_MagMap" [buffer,mapbuf] Nothing 1 (Special 0) NoId

-- | subtract spectral energy
--
--  PV_MagMinus [KR] bufferA=0.0 bufferB=0.0 remove=1.0
pv_MagMinus :: UGen -> UGen -> UGen -> UGen
pv_MagMinus bufferA bufferB remove = mkUGen Nothing [KR] (Left KR) "PV_MagMinus" [bufferA,bufferB,remove] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagMulAdd [KR] buffer=0.0
pv_MagMulAdd :: UGen -> UGen
pv_MagMulAdd buffer = mkUGen Nothing [KR] (Left KR) "PV_MagMulAdd" [buffer] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagScale [KR] bufferA=0.0 bufferB=0.0
pv_MagScale :: UGen -> UGen -> UGen
pv_MagScale bufferA bufferB = mkUGen Nothing [KR] (Left KR) "PV_MagScale" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Smooth spectral magnitudes over time
--
--  PV_MagSmooth [KR] buffer=0.0 factor=0.1
pv_MagSmooth :: UGen -> UGen -> UGen
pv_MagSmooth buffer factor = mkUGen Nothing [KR] (Left KR) "PV_MagSmooth" [buffer,factor] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagSubtract [KR] bufferA=0.0 bufferB=0.0 zerolimit=0.0
pv_MagSubtract :: UGen -> UGen -> UGen -> UGen
pv_MagSubtract bufferA bufferB zerolimit = mkUGen Nothing [KR] (Left KR) "PV_MagSubtract" [bufferA,bufferB,zerolimit] Nothing 1 (Special 0) NoId

-- | Return the N strongest bins
--
--  PV_MaxMagN [KR] buffer=0.0 numbins=0.0
pv_MaxMagN :: UGen -> UGen -> UGen
pv_MaxMagN buffer numbins = mkUGen Nothing [KR] (Left KR) "PV_MaxMagN" [buffer,numbins] Nothing 1 (Special 0) NoId

-- | Return the N weakest bins
--
--  PV_MinMagN [KR] buffer=0.0 numbins=0.0
pv_MinMagN :: UGen -> UGen -> UGen
pv_MinMagN buffer numbins = mkUGen Nothing [KR] (Left KR) "PV_MinMagN" [buffer,numbins] Nothing 1 (Special 0) NoId

-- | one kind of spectral morphing
--
--  PV_Morph [KR] bufferA=0.0 bufferB=0.0 morph=0.0
pv_Morph :: UGen -> UGen -> UGen -> UGen
pv_Morph bufferA bufferB morph = mkUGen Nothing [KR] (Left KR) "PV_Morph" [bufferA,bufferB,morph] Nothing 1 (Special 0) NoId

-- | Return only bins that are unstable
--
--  PV_NoiseSynthF [KR] buffer=0.0 threshold=0.1 numFrames=2.0 initflag=0.0
pv_NoiseSynthF :: UGen -> UGen -> UGen -> UGen -> UGen
pv_NoiseSynthF buffer threshold numFrames initflag = mkUGen Nothing [KR] (Left KR) "PV_NoiseSynthF" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | Return only bins that are unstable
--
--  PV_NoiseSynthP [KR] buffer=0.0 threshold=0.1 numFrames=2.0 initflag=0.0
pv_NoiseSynthP :: UGen -> UGen -> UGen -> UGen -> UGen
pv_NoiseSynthP buffer threshold numFrames initflag = mkUGen Nothing [KR] (Left KR) "PV_NoiseSynthP" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | Return the odd numbered bins in an FFT buffer
--
--  PV_OddBin [KR] buffer=0.0
pv_OddBin :: UGen -> UGen
pv_OddBin buffer = mkUGen Nothing [KR] (Left KR) "PV_OddBin" [buffer] Nothing 1 (Special 0) NoId

-- | Return only bins that are stable
--
--  PV_PartialSynthF [KR] buffer=0.0 threshold=0.1 numFrames=2.0 initflag=0.0
pv_PartialSynthF :: UGen -> UGen -> UGen -> UGen -> UGen
pv_PartialSynthF buffer threshold numFrames initflag = mkUGen Nothing [KR] (Left KR) "PV_PartialSynthF" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | Return only bins that are stable
--
--  PV_PartialSynthP [KR] buffer=0.0 threshold=0.1 numFrames=2.0 initflag=0.0
pv_PartialSynthP :: UGen -> UGen -> UGen -> UGen -> UGen
pv_PartialSynthP buffer threshold numFrames initflag = mkUGen Nothing [KR] (Left KR) "PV_PartialSynthP" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_PitchShift [KR] buffer=0.0 ratio=0.0
pv_PitchShift :: UGen -> UGen -> UGen
pv_PitchShift buffer ratio = mkUGen Nothing [KR] (Left KR) "PV_PitchShift" [buffer,ratio] Nothing 1 (Special 0) NoId

-- | Plays FFT data to a memory buffer
--
--  PV_PlayBuf [KR] buffer=0.0 playbuf=0.0 rate=1.0 offset=0.0 loop=0.0
pv_PlayBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_PlayBuf buffer playbuf_ rate_ offset loop = mkUGen Nothing [KR] (Left KR) "PV_PlayBuf" [buffer,playbuf_,rate_,offset,loop] Nothing 1 (Special 0) NoId

-- | Records FFT data to a memory buffer
--
--  PV_RecordBuf [KR] buffer=0.0 recbuf=0.0 offset=0.0 run=0.0 loop=0.0 hop=0.5 wintype=0.0
pv_RecordBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RecordBuf buffer recbuf offset run loop hop wintype = mkUGen Nothing [KR] (Left KR) "PV_RecordBuf" [buffer,recbuf,offset,run,loop,hop,wintype] Nothing 1 (Special 0) NoId

-- | combine low and high bins from two inputs with interpolation
--
--  PV_SoftWipe [KR] bufferA=0.0 bufferB=0.0 wipe=0.0
pv_SoftWipe :: UGen -> UGen -> UGen -> UGen
pv_SoftWipe bufferA bufferB wipe = mkUGen Nothing [KR] (Left KR) "PV_SoftWipe" [bufferA,bufferB,wipe] Nothing 1 (Special 0) NoId

-- | A harmonic enhancer
--
--  PV_SpectralEnhance [KR] buffer=0.0 numPartials=8.0 ratio=2.0 strength=0.1
pv_SpectralEnhance :: UGen -> UGen -> UGen -> UGen -> UGen
pv_SpectralEnhance buffer numPartials ratio strength = mkUGen Nothing [KR] (Left KR) "PV_SpectralEnhance" [buffer,numPartials,ratio,strength] Nothing 1 (Special 0) NoId

-- | Maps the spectral envelope of one FFT process onto another
--
--  PV_SpectralMap [KR] buffer=0.0 specBuffer=0.0 floor=0.0 freeze=0.0 mode=0.0 norm=0.0 window=0.0
pv_SpectralMap :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_SpectralMap buffer specBuffer floor_ freeze mode norm window = mkUGen Nothing [KR] (Left KR) "PV_SpectralMap" [buffer,specBuffer,floor_,freeze,mode,norm,window] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_Whiten [KR] chain=0.0 trackbufnum=0.0 relaxtime=2.0 floor=0.1 smear=0.0 bindownsample=0.0
pv_Whiten :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_Whiten chain trackbufnum relaxtime floor_ smear bindownsample = mkUGen Nothing [KR] (Left KR) "PV_Whiten" [chain,trackbufnum,relaxtime,floor_,smear,bindownsample] Nothing 1 (Special 0) NoId

-- | one kind of spectral morphing
--
--  PV_XFade [KR] bufferA=0.0 bufferB=0.0 fade=0.0
pv_xFade :: UGen -> UGen -> UGen -> UGen
pv_xFade bufferA bufferB fade = mkUGen Nothing [KR] (Left KR) "PV_XFade" [bufferA,bufferB,fade] Nothing 1 (Special 0) NoId

-- | Equal power pan across an array of speakers
--
--  PanX [KR,AR] numChans=0.0 in=0.0 pos=0.0 level=1.0 width=2.0
panX :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
panX rate numChans in_ pos level width = mkUGen Nothing [KR,AR] (Left rate) "PanX" [numChans,in_,pos,level,width] Nothing 0 (Special 0) NoId

-- | (Undocumented class)
--
--  PanX2D [KR,AR] numChansX=0.0 numChansY=0.0 in=0.0 posX=0.0 posY=0.0 level=1.0 widthX=2.0 widthY=2.0
panX2D :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
panX2D rate numChansX numChansY in_ posX posY level widthX widthY = mkUGen Nothing [KR,AR] (Left rate) "PanX2D" [numChansX,numChansY,in_,posX,posY,level,widthX,widthY] Nothing 0 (Special 0) NoId

-- | (Undocumented class)
--
--  PeakEQ2 [AR] in=0.0 freq=1200.0 rs=1.0 db=0.0
peakEQ2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
peakEQ2 rate in_ freq rs db = mkUGen Nothing [AR] (Left rate) "PeakEQ2" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PeakEQ4 [AR] in=0.0 freq=1200.0 rs=1.0 db=0.0
peakEQ4 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
peakEQ4 rate in_ freq rs db = mkUGen Nothing [AR] (Left rate) "PeakEQ4" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | 3D Perlin Noise
--
--  Perlin3 [KR,AR] x=0.0 y=0.0 z=0.0
perlin3 :: Rate -> UGen -> UGen -> UGen -> UGen
perlin3 rate x y z = mkUGen Nothing [KR,AR] (Left rate) "Perlin3" [x,y,z] Nothing 1 (Special 0) NoId

-- | Sample permutation UGen.
--
--  PermMod [AR] in=0.0 freq=100.0
permMod :: Rate -> UGen -> UGen -> UGen
permMod rate in_ freq = mkUGen Nothing [AR] (Left rate) "PermMod" [in_,freq] Nothing 1 (Special 0) NoId

-- | Sample permutation UGen with programmable pattern.
--
--  PermModArray [AR] in=0.0 freq=50.0 pattern=0.0
permModArray :: Rate -> UGen -> UGen -> UGen -> UGen
permModArray rate in_ freq pattern = mkUGen Nothing [AR] (Left rate) "PermModArray" [in_,freq,pattern] Nothing 1 (Special 0) NoId

-- | Sample permutation UGen with tail.
--
--  PermModT [AR] in=0.0 outfreq=440.0 infreq=5000.0
permModT :: Rate -> UGen -> UGen -> UGen -> UGen
permModT rate in_ outfreq infreq = mkUGen Nothing [AR] (Left rate) "PermModT" [in_,outfreq,infreq] Nothing 1 (Special 0) NoId

-- | Tree classifier using (hyper)planes – UGen or language-side
--
--  PlaneTree [KR] treebuf=0.0 in=0.0 gate=1.0
planeTree :: Rate -> UGen -> UGen -> UGen -> UGen
planeTree rate treebuf in_ gate_ = mkUGen Nothing [KR] (Left rate) "PlaneTree" [treebuf,in_,gate_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PosRatio [AR] in=0.0 period=100.0 thresh=0.1
posRatio :: Rate -> UGen -> UGen -> UGen -> UGen
posRatio rate in_ period thresh = mkUGen Nothing [AR] (Left rate) "PosRatio" [in_,period,thresh] Nothing 1 (Special 0) NoId

-- | debug assistance
--
--  PrintVal [KR] in=0.0 numblocks=100.0 id=0.0
printVal :: Rate -> UGen -> UGen -> UGen -> UGen
printVal rate in_ numblocks id_ = mkUGen Nothing [KR] (Left rate) "PrintVal" [in_,numblocks,id_] Nothing 1 (Special 0) NoId

-- | constant Q transform pitch follower
--
--  Qitch [KR] in=0.0 databufnum=0.0 ampThreshold=1.0e-2 algoflag=1.0 ampbufnum=0.0 minfreq=0.0 maxfreq=2500.0
qitch :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
qitch rate in_ databufnum ampThreshold algoflag ampbufnum minfreq maxfreq = mkUGen Nothing [KR] (Left rate) "Qitch" [in_,databufnum,ampThreshold,algoflag,ampbufnum,minfreq,maxfreq] Nothing 2 (Special 0) NoId

-- | TB303 Filter Emulation
--
--  RLPFD [KR,AR] in=0.0 ffreq=440.0 res=0.0 dist=0.0;    FILTER: TRUE
rlpfd :: UGen -> UGen -> UGen -> UGen -> UGen
rlpfd in_ ffreq res dist = mkUGen Nothing [KR,AR] (Right [0]) "RLPFD" [in_,ffreq,res,dist] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMAFoodChainL [AR] freq=22050.0 a1=5.0 b1=3.0 d1=0.4 a2=0.1 b2=2.0 d2=1.0e-2 k=1.0943 r=0.8904 h=5.0e-2 xi=0.1 yi=0.0 zi=0.0
rmaFoodChainL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rmaFoodChainL rate freq a1 b1 d1 a2 b2 d2 k r h xi yi zi = mkUGen Nothing [AR] (Left rate) "RMAFoodChainL" [freq,a1,b1,d1,a2,b2,d2,k,r,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  RMEQ [AR] in=0.0 freq=440.0 rq=0.1 k=0.0
rmeq :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
rmeq rate in_ freq rq k = mkUGen Nothing [AR] (Left rate) "RMEQ" [in_,freq,rq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMEQSuite [] maxSize=0.0
rmeqSuite :: Rate -> UGen -> UGen
rmeqSuite rate maxSize = mkUGen Nothing [IR,KR,AR,DR] (Left rate) "RMEQSuite" [maxSize] Nothing 1 (Special 0) NoId

-- | root mean square
--
--  RMS [KR,AR] in=0.0 lpFreq=10.0
rms :: Rate -> UGen -> UGen -> UGen
rms rate in_ lpFreq = mkUGen Nothing [KR,AR] (Left rate) "RMS" [in_,lpFreq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMShelf [AR] in=0.0 freq=440.0 k=0.0
rmShelf :: Rate -> UGen -> UGen -> UGen -> UGen
rmShelf rate in_ freq k = mkUGen Nothing [AR] (Left rate) "RMShelf" [in_,freq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMShelf2 [AR] in=0.0 freq=440.0 k=0.0
rmShelf2 :: Rate -> UGen -> UGen -> UGen -> UGen
rmShelf2 rate in_ freq k = mkUGen Nothing [AR] (Left rate) "RMShelf2" [in_,freq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RegaliaMitraEQ [AR] in=0.0 freq=440.0 rq=0.1 k=0.0
regaliaMitraEQ :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
regaliaMitraEQ rate in_ freq rq k = mkUGen Nothing [AR] (Left rate) "RegaliaMitraEQ" [in_,freq,rq,k] Nothing 1 (Special 0) NoId

-- | A modal synthesis voice
--
--  Rongs [AR] trigger=0.0 sustain=1.0 f0=1.0e-2 structure=0.5 brightness=0.5 damping=0.75 accent=0.9 stretch=0.5 position=0.15 loss=0.15
rongs :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rongs rate trigger sustain f0 structure brightness damping accent stretch position loss = mkUGen Nothing [AR] (Left rate) "Rongs" [trigger,sustain,f0,structure,brightness,damping,accent,stretch,position,loss] Nothing 1 (Special 0) NoId

-- | Rossler chaotic generator
--
--  RosslerL [AR] freq=22050.0 a=0.2 b=0.2 c=5.7 h=5.0e-2 xi=0.1 yi=0.0 zi=0.0
rosslerL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rosslerL rate freq a b c h xi yi zi = mkUGen Nothing [AR] (Left rate) "RosslerL" [freq,a,b,c,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  RosslerResL [AR] in=0.0 stiff=1.0 freq=22050.0 a=0.2 b=0.2 c=5.7 h=5.0e-2 xi=0.1 yi=0.0 zi=0.0
rosslerResL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rosslerResL rate in_ stiff freq a b c h xi yi zi = mkUGen Nothing [AR] (Left rate) "RosslerResL" [in_,stiff,freq,a,b,c,h,xi,yi,zi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Rotate [AR] w=0.0 x=0.0 y=0.0 z=0.0 rotate=0.0
rotate :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rotate rate w x y z rotate_ = mkUGen Nothing [AR] (Left rate) "Rotate" [w,x,y,z,rotate_] Nothing 1 (Special 0) NoId

-- | experimental time domain onset detector
--
--  SLOnset [KR] input=0.0 memorysize1=20.0 before=5.0 after=5.0 threshold=10.0 hysteresis=10.0
slOnset :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
slOnset rate input memorysize1 before after threshold hysteresis = mkUGen Nothing [KR] (Left rate) "SLOnset" [input,memorysize1,before,after,threshold,hysteresis] Nothing 1 (Special 0) NoId

-- | Spectral Modeling Synthesis
--
--  SMS [AR] input=0.0 maxpeaks=80.0 currentpeaks=80.0 tolerance=4.0 noisefloor=0.2 freqmult=1.0 freqadd=0.0 formantpreserve=0.0 useifft=0.0 ampmult=1.0 graphicsbufnum=0.0;    FILTER: TRUE
sms :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sms input maxpeaks currentpeaks tolerance noisefloor freqmult freqadd formantpreserve useifft ampmult graphicsbufnum = mkUGen Nothing [AR] (Right [0]) "SMS" [input,maxpeaks,currentpeaks,tolerance,noisefloor,freqmult,freqadd,formantpreserve,useifft,ampmult,graphicsbufnum] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  SOMAreaWr [KR] bufnum=0.0 inputdata=0.0 coords=0.0 netsize=10.0 numdims=2.0 nhood=0.5 gate=1.0
somAreaWr :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
somAreaWr rate bufnum inputdata coords netsize numdims nhood gate_ = mkUGen Nothing [KR] (Left rate) "SOMAreaWr" [bufnum,inputdata,coords,netsize,numdims,nhood,gate_] Nothing 1 (Special 0) NoId

-- | Map an input using a Self-Organising Map
--
--  SOMRd [KR,AR] bufnum=0.0 inputdata=0.0 netsize=10.0 numdims=2.0 gate=1.0
somRd :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
somRd rate bufnum inputdata netsize numdims gate_ = mkUGen Nothing [KR,AR] (Left rate) "SOMRd" [bufnum,inputdata,netsize,numdims,gate_] Nothing 2 (Special 0) NoId

-- | Create (train) a Self-Organising Map
--
--  SOMTrain [KR] bufnum=0.0 inputdata=0.0 netsize=10.0 numdims=2.0 traindur=5000.0 nhood=0.5 gate=1.0 initweight=1.0
somTrain :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
somTrain rate bufnum inputdata netsize numdims traindur nhood gate_ initweight = mkUGen Nothing [KR] (Left rate) "SOMTrain" [bufnum,inputdata,netsize,numdims,traindur,nhood,gate_,initweight] Nothing 3 (Special 0) NoId

-- | 12db/Oct State Variable Filter
--
--  SVF [KR,AR] signal=0.0 cutoff=2200.0 res=0.1 lowpass=1.0 bandpass=0.0 highpass=0.0 notch=0.0 peak=0.0;    FILTER: TRUE
svf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
svf signal cutoff res lowpass bandpass highpass notch peak_ = mkUGen Nothing [KR,AR] (Right [0]) "SVF" [signal,cutoff,res,lowpass,bandpass,highpass,notch,peak_] Nothing 1 (Special 0) NoId

-- | super-efficient sawtooth oscillator with low aliasing
--
--  SawDPW [KR,AR] freq=440.0 iphase=0.0
sawDPW :: Rate -> UGen -> UGen -> UGen
sawDPW rate freq iphase = mkUGen Nothing [KR,AR] (Left rate) "SawDPW" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Perceptual feature modeling sensory dissonance
--
--  SensoryDissonance [KR] fft=0.0 maxpeaks=100.0 peakthreshold=0.1 norm=0.0 clamp=1.0
sensoryDissonance :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sensoryDissonance rate fft_ maxpeaks peakthreshold norm clamp = mkUGen Nothing [KR] (Left rate) "SensoryDissonance" [fft_,maxpeaks,peakthreshold,norm,clamp] Nothing 1 (Special 0) NoId

-- | Fuzzy sieve based synthesis
--
--  Sieve1 [KR,AR] bufnum=0.0 gap=2.0 alternate=1.0
sieve1 :: Rate -> UGen -> UGen -> UGen -> UGen
sieve1 rate bufnum gap alternate = mkUGen Nothing [KR,AR] (Left rate) "Sieve1" [bufnum,gap,alternate] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains
--
--  SinGrain [AR] trigger=0.0 dur=1.0 freq=440.0
sinGrain :: Rate -> UGen -> UGen -> UGen -> UGen
sinGrain rate trigger dur freq = mkUGen Nothing [AR] (Left rate) "SinGrain" [trigger,dur,freq] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains and user supplied envelope
--
--  SinGrainB [AR] trigger=0.0 dur=1.0 freq=440.0 envbuf=0.0
sinGrainB :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
sinGrainB rate trigger dur freq envbuf = mkUGen Nothing [AR] (Left rate) "SinGrainB" [trigger,dur,freq,envbuf] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains with Ambisonic panning and user supplied envelope
--
--  SinGrainBBF [AR] trigger=0.0 dur=1.0 freq=440.0 envbuf=0.0 azimuth=0.0 elevation=0.0 rho=1.0 wComp=0.0
sinGrainBBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sinGrainBBF rate trigger dur freq envbuf azimuth elevation rho wComp = mkUGen Nothing [AR] (Left rate) "SinGrainBBF" [trigger,dur,freq,envbuf,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains with Ambisonic panning
--
--  SinGrainBF [AR] trigger=0.0 dur=1.0 freq=440.0 azimuth=0.0 elevation=0.0 rho=1.0 wComp=0.0
sinGrainBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sinGrainBF rate trigger dur freq azimuth elevation rho wComp = mkUGen Nothing [AR] (Left rate) "SinGrainBF" [trigger,dur,freq,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains and user supplied envelopes
--
--  SinGrainI [AR] trigger=0.0 dur=1.0 freq=440.0 envbuf1=0.0 envbuf2=0.0 ifac=0.5
sinGrainI :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sinGrainI rate trigger dur freq envbuf1 envbuf2 ifac = mkUGen Nothing [AR] (Left rate) "SinGrainI" [trigger,dur,freq,envbuf1,envbuf2,ifac] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains with Ambisonic panning and user supplied envelopes
--
--  SinGrainIBF [AR] trigger=0.0 dur=1.0 freq=440.0 envbuf1=0.0 envbuf2=0.0 ifac=0.5 azimuth=0.0 elevation=0.0 rho=1.0 wComp=0.0
sinGrainIBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sinGrainIBF rate trigger dur freq envbuf1 envbuf2 ifac azimuth elevation rho wComp = mkUGen Nothing [AR] (Left rate) "SinGrainIBF" [trigger,dur,freq,envbuf1,envbuf2,ifac,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  SinTone [AR] freq=440.0 phase=0.0
sinTone :: Rate -> UGen -> UGen -> UGen
sinTone rate freq phase = mkUGen Nothing [AR] (Left rate) "SinTone" [freq,phase] Nothing 1 (Special 0) NoId

-- | port of some ladspa plugins
--
--  SineShaper [AR] in=0.0 limit=1.0;    FILTER: TRUE
sineShaper :: UGen -> UGen -> UGen
sineShaper in_ limit = mkUGen Nothing [AR] (Right [0]) "SineShaper" [in_,limit] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SkipNeedle [AR] range=44100.0 rate=10.0 offset=0.0
skipNeedle :: Rate -> UGen -> UGen -> UGen -> UGen
skipNeedle rate range rate_ offset = mkUGen Nothing [AR] (Left rate) "SkipNeedle" [range,rate_,offset] Nothing 1 (Special 0) NoId

-- | Smooth samplerate and bitrate reduction
--
--  SmoothDecimator [AR] in=0.0 rate=44100.0 smoothing=0.5
smoothDecimator :: Rate -> UGen -> UGen -> UGen -> UGen
smoothDecimator rate in_ rate_ smoothing = mkUGen Nothing [AR] (Left rate) "SmoothDecimator" [in_,rate_,smoothing] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipAmp [AR] in=0.0 pregain=1.0;    FILTER: TRUE
softClipAmp :: UGen -> UGen -> UGen
softClipAmp in_ pregain = mkUGen Nothing [AR] (Right [0]) "SoftClipAmp" [in_,pregain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipAmp4 [AR] in=0.0 pregain=1.0;    FILTER: TRUE
softClipAmp4 :: UGen -> UGen -> UGen
softClipAmp4 in_ pregain = mkUGen Nothing [AR] (Right [0]) "SoftClipAmp4" [in_,pregain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipAmp8 [AR] in=0.0 pregain=1.0;    FILTER: TRUE
softClipAmp8 :: UGen -> UGen -> UGen
softClipAmp8 in_ pregain = mkUGen Nothing [AR] (Right [0]) "SoftClipAmp8" [in_,pregain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipper4 [AR] in=0.0
softClipper4 :: Rate -> UGen -> UGen
softClipper4 rate in_ = mkUGen Nothing [AR] (Left rate) "SoftClipper4" [in_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipper8 [AR] in=0.0
softClipper8 :: Rate -> UGen -> UGen
softClipper8 rate in_ = mkUGen Nothing [AR] (Left rate) "SoftClipper8" [in_] Nothing 1 (Special 0) NoId

-- | Karplus-Strong via a sorting algorithm
--
--  SortBuf [AR] bufnum=0.0 sortrate=10.0 reset=0.0
sortBuf :: Rate -> UGen -> UGen -> UGen -> UGen
sortBuf rate bufnum sortrate reset = mkUGen Nothing [AR] (Left rate) "SortBuf" [bufnum,sortrate,reset] Nothing 1 (Special 0) NoId

-- | Spectral feature extraction
--
--  SpectralEntropy [KR] fft=0.0 fftsize=2048.0 numbands=1.0;    NC INPUT: True
spectralEntropy :: Int -> Rate -> UGen -> UGen -> UGen -> UGen
spectralEntropy numChannels rate fft_ fftsize numbands = mkUGen Nothing [KR] (Left rate) "SpectralEntropy" [fft_,fftsize,numbands] Nothing numChannels (Special 0) NoId

-- | (Undocumented class)
--
--  Spreader [AR] in=0.0 theta=1.5707963267949 filtsPerOctave=8.0
spreader :: Rate -> UGen -> UGen -> UGen -> UGen
spreader rate in_ theta filtsPerOctave = mkUGen Nothing [AR] (Left rate) "Spreader" [in_,theta,filtsPerOctave] Nothing 2 (Special 0) NoId

-- | Spruce bud worm model equations
--
--  SpruceBudworm [AR] reset=0.0 rate=0.1 k1=27.9 k2=1.5 alpha=0.1 beta=10.1 mu=0.3 rho=10.1 initx=0.9 inity=0.1
spruceBudworm :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
spruceBudworm rate reset rate_ k1 k2 alpha beta mu rho initx inity = mkUGen Nothing [AR] (Left rate) "SpruceBudworm" [reset,rate_,k1,k2,alpha,beta,mu,rho,initx,inity] Nothing 2 (Special 0) NoId

-- | Wave squeezer. Maybe a kind of pitch shifter.
--
--  Squiz [KR,AR] in=0.0 pitchratio=2.0 zcperchunk=1.0 memlen=0.1;    FILTER: TRUE
squiz :: UGen -> UGen -> UGen -> UGen -> UGen
squiz in_ pitchratio zcperchunk memlen = mkUGen Nothing [KR,AR] (Right [0]) "Squiz" [in_,pitchratio,zcperchunk,memlen] Nothing 1 (Special 0) NoId

-- | standard map 2D chaotic generator
--
--  Standard2DC [KR,AR] minfreq=11025.0 maxfreq=22050.0 k=1.4 x0=4.9789799812499 y0=5.7473416156381
standard2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
standard2DC rate minfreq maxfreq k x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Standard2DC" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | standard map 2D chaotic generator
--
--  Standard2DL [KR,AR] minfreq=11025.0 maxfreq=22050.0 k=1.4 x0=4.9789799812499 y0=5.7473416156381
standard2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
standard2DL rate minfreq maxfreq k x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Standard2DL" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | standard map 2D chaotic generator
--
--  Standard2DN [KR,AR] minfreq=11025.0 maxfreq=22050.0 k=1.4 x0=4.9789799812499 y0=5.7473416156381
standard2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
standard2DN rate minfreq maxfreq k x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "Standard2DN" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StandardTrig [KR,AR] minfreq=5.0 maxfreq=10.0 k=1.4 x0=4.9789799812499 y0=5.7473416156381
standardTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
standardTrig rate minfreq maxfreq k x0 y0 = mkUGen Nothing [KR,AR] (Left rate) "StandardTrig" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBandedWG [KR,AR] freq=440.0 instr=0.0 bowpressure=0.0 bowmotion=0.0 integration=0.0 modalresonance=64.0 bowvelocity=0.0 setstriking=0.0 trig=1.0
stkBandedWG :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkBandedWG rate freq instr bowpressure bowmotion integration modalresonance bowvelocity setstriking trig_ = mkUGen Nothing [KR,AR] (Left rate) "StkBandedWG" [freq,instr,bowpressure,bowmotion,integration,modalresonance,bowvelocity,setstriking,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBeeThree [KR,AR] freq=440.0 op4gain=10.0 op3gain=20.0 lfospeed=64.0 lfodepth=0.0 adsrtarget=64.0 trig=1.0
stkBeeThree :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkBeeThree rate freq op4gain op3gain lfospeed lfodepth adsrtarget trig_ = mkUGen Nothing [KR,AR] (Left rate) "StkBeeThree" [freq,op4gain,op3gain,lfospeed,lfodepth,adsrtarget,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBlowHole [KR,AR] freq=440.0 reedstiffness=64.0 noisegain=20.0 tonehole=64.0 register=11.0 breathpressure=64.0
stkBlowHole :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkBlowHole rate freq reedstiffness noisegain tonehole register breathpressure = mkUGen Nothing [KR,AR] (Left rate) "StkBlowHole" [freq,reedstiffness,noisegain,tonehole,register,breathpressure] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBowed [KR,AR] freq=220.0 bowpressure=64.0 bowposition=64.0 vibfreq=64.0 vibgain=64.0 loudness=64.0 gate=1.0 attackrate=1.0 decayrate=1.0
stkBowed :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkBowed rate freq bowpressure bowposition vibfreq vibgain loudness_ gate_ attackrate decayrate = mkUGen Nothing [KR,AR] (Left rate) "StkBowed" [freq,bowpressure,bowposition,vibfreq,vibgain,loudness_,gate_,attackrate,decayrate] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkClarinet [KR,AR] freq=440.0 reedstiffness=64.0 noisegain=4.0 vibfreq=64.0 vibgain=11.0 breathpressure=64.0 trig=1.0
stkClarinet :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkClarinet rate freq reedstiffness noisegain vibfreq vibgain breathpressure trig_ = mkUGen Nothing [KR,AR] (Left rate) "StkClarinet" [freq,reedstiffness,noisegain,vibfreq,vibgain,breathpressure,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkFlute [KR,AR] freq=440.0 jetDelay=49.0 noisegain=0.15 jetRatio=0.32
stkFlute :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
stkFlute rate freq jetDelay noisegain jetRatio = mkUGen Nothing [KR,AR] (Left rate) "StkFlute" [freq,jetDelay,noisegain,jetRatio] Nothing 1 (Special 0) NoId

-- | Wrapping Synthesis toolkit.
--
--  StkGlobals [AR] showWarnings=0.0 printErrors=0.0 rawfilepath=0.0
stkGlobals :: Rate -> UGen -> UGen -> UGen -> UGen
stkGlobals rate showWarnings printErrors rawfilepath = mkUGen Nothing [AR] (Left rate) "StkGlobals" [showWarnings,printErrors,rawfilepath] Nothing 1 (Special 0) NoId

-- | Wrapping Synthesis toolkit.
--
--  StkInst [AR] freq=220.0 gate=1.0 onamp=1.0 offamp=0.5 instNumber=6.0 *args=0.0;    MCE=1, REORDERS INPUTS: [4,0,1,2,3,5]
stkInst :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkInst rate freq gate_ onamp offamp instNumber args = mkUGen Nothing [AR] (Left rate) "StkInst" [freq,gate_,onamp,offamp,instNumber] (Just [args]) 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkMandolin [KR,AR] freq=520.0 bodysize=64.0 pickposition=64.0 stringdamping=69.0 stringdetune=10.0 aftertouch=64.0 trig=1.0
stkMandolin :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkMandolin rate freq bodysize pickposition stringdamping stringdetune aftertouch trig_ = mkUGen Nothing [KR,AR] (Left rate) "StkMandolin" [freq,bodysize,pickposition,stringdamping,stringdetune,aftertouch,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkModalBar [KR,AR] freq=440.0 instrument=0.0 stickhardness=64.0 stickposition=64.0 vibratogain=20.0 vibratofreq=20.0 directstickmix=64.0 volume=64.0 trig=1.0
stkModalBar :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkModalBar rate freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume trig_ = mkUGen Nothing [KR,AR] (Left rate) "StkModalBar" [freq,instrument,stickhardness,stickposition,vibratogain,vibratofreq,directstickmix,volume,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkMoog [KR,AR] freq=440.0 filterQ=10.0 sweeprate=20.0 vibfreq=64.0 vibgain=0.0 gain=64.0 trig=1.0
stkMoog :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkMoog rate freq filterQ sweeprate vibfreq vibgain gain trig_ = mkUGen Nothing [KR,AR] (Left rate) "StkMoog" [freq,filterQ,sweeprate,vibfreq,vibgain,gain,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkPluck [KR,AR] freq=440.0 decay=0.99
stkPluck :: Rate -> UGen -> UGen -> UGen
stkPluck rate freq decay_ = mkUGen Nothing [KR,AR] (Left rate) "StkPluck" [freq,decay_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkSaxofony [KR,AR] freq=220.0 reedstiffness=64.0 reedaperture=64.0 noisegain=20.0 blowposition=26.0 vibratofrequency=20.0 vibratogain=20.0 breathpressure=128.0 trig=1.0
stkSaxofony :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkSaxofony rate freq reedstiffness reedaperture noisegain blowposition vibratofrequency vibratogain breathpressure trig_ = mkUGen Nothing [KR,AR] (Left rate) "StkSaxofony" [freq,reedstiffness,reedaperture,noisegain,blowposition,vibratofrequency,vibratogain,breathpressure,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkShakers [KR,AR] instr=0.0 energy=64.0 decay=64.0 objects=64.0 resfreq=64.0
stkShakers :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkShakers rate instr energy decay_ objects resfreq = mkUGen Nothing [KR,AR] (Left rate) "StkShakers" [instr,energy,decay_,objects,resfreq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkVoicForm [KR,AR] freq=440.0 vuvmix=64.0 vowelphon=64.0 vibfreq=64.0 vibgain=20.0 loudness=64.0 trig=1.0
stkVoicForm :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkVoicForm rate freq vuvmix vowelphon vibfreq vibgain loudness_ trig_ = mkUGen Nothing [KR,AR] (Left rate) "StkVoicForm" [freq,vuvmix,vowelphon,vibfreq,vibgain,loudness_,trig_] Nothing 1 (Special 0) NoId

-- | String resonance filter
--
--  Streson [KR,AR] input=0.0 delayTime=3.0e-3 res=0.9;    FILTER: TRUE
streson :: UGen -> UGen -> UGen -> UGen
streson input delayTime res = mkUGen Nothing [KR,AR] (Right [0]) "Streson" [input,delayTime,res] Nothing 1 (Special 0) NoId

-- | Pulse counter with floating point steps
--
--  Summer [KR,AR] trig=0.0 step=1.0 reset=0.0 resetval=0.0;    FILTER: TRUE
summer :: UGen -> UGen -> UGen -> UGen -> UGen
summer trig_ step reset resetval = mkUGen Nothing [KR,AR] (Right [0]) "Summer" [trig_,step,reset,resetval] Nothing 1 (Special 0) NoId

-- | feedback delay line implementing switch-and-ramp buffer jumping
--
--  SwitchDelay [AR] in=0.0 drylevel=1.0 wetlevel=1.0 delaytime=1.0 delayfactor=0.7 maxdelaytime=20.0;    FILTER: TRUE
switchDelay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
switchDelay in_ drylevel wetlevel delaytime delayfactor maxdelaytime = mkUGen Nothing [AR] (Right [0]) "SwitchDelay" [in_,drylevel,wetlevel,delaytime,delayfactor,maxdelaytime] Nothing 1 (Special 0) NoId

-- | triggered beta random distribution
--
--  TBetaRand [KR,AR] lo=0.0 hi=1.0 prob1=0.0 prob2=0.0 trig=0.0;    FILTER: TRUE, NONDET
tBetaRand :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tBetaRand z lo hi prob1 prob2 trig_ = mkUGen Nothing [KR,AR] (Right [4]) "TBetaRand" [lo,hi,prob1,prob2,trig_] Nothing 1 (Special 0) (toUId z)

-- | triggered random walk generator
--
--  TBrownRand [KR,AR] lo=0.0 hi=1.0 dev=1.0 dist=0.0 trig=0.0;    FILTER: TRUE, NONDET
tBrownRand :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tBrownRand z lo hi dev dist trig_ = mkUGen Nothing [KR,AR] (Right [4]) "TBrownRand" [lo,hi,dev,dist,trig_] Nothing 1 (Special 0) (toUId z)

-- | triggered gaussian random distribution
--
--  TGaussRand [KR,AR] lo=0.0 hi=1.0 trig=0.0;    FILTER: TRUE, NONDET
tGaussRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tGaussRand z lo hi trig_ = mkUGen Nothing [KR,AR] (Right [2]) "TGaussRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | buffer granulator with linear att/dec
--
--  TGrains2 [AR] trigger=0.0 bufnum=0.0 rate=1.0 centerPos=0.0 dur=0.1 pan=0.0 amp=0.1 att=0.5 dec=0.5 interp=4.0;    NC INPUT: True
tGrains2 :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains2 numChannels rate trigger bufnum rate_ centerPos dur pan amp att dec interp = mkUGen Nothing [AR] (Left rate) "TGrains2" [trigger,bufnum,rate_,centerPos,dur,pan,amp,att,dec,interp] Nothing numChannels (Special 0) NoId

-- | buffer granulator with user envelope
--
--  TGrains3 [AR] trigger=0.0 bufnum=0.0 rate=1.0 centerPos=0.0 dur=0.1 pan=0.0 amp=0.1 att=0.5 dec=0.5 window=1.0 interp=4.0;    NC INPUT: True
tGrains3 :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains3 numChannels rate trigger bufnum rate_ centerPos dur pan amp att dec window interp = mkUGen Nothing [AR] (Left rate) "TGrains3" [trigger,bufnum,rate_,centerPos,dur,pan,amp,att,dec,window,interp] Nothing numChannels (Special 0) NoId

-- | Tracking Phase Vocoder
--
--  TPV [AR] chain=0.0 windowsize=1024.0 hopsize=512.0 maxpeaks=80.0 currentpeaks=0.0 freqmult=1.0 tolerance=4.0 noisefloor=0.2
tpv :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tpv chain windowsize hopsize maxpeaks currentpeaks freqmult tolerance noisefloor = mkUGen Nothing [AR] (Left AR) "TPV" [chain,windowsize,hopsize,maxpeaks,currentpeaks,freqmult,tolerance,noisefloor] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  TTendency [KR,AR] trigger=0.0 dist=0.0 parX=0.0 parY=1.0 parA=0.0 parB=0.0
tTendency :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tTendency rate trigger dist parX parY parA parB = mkUGen Nothing [KR,AR] (Left rate) "TTendency" [trigger,dist,parX,parY,parA,parB] Nothing 1 (Special 0) NoId

-- | pitch tracker
--
--  Tartini [KR] in=0.0 threshold=0.93 n=2048.0 k=0.0 overlap=1024.0 smallCutoff=0.5
tartini :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tartini rate in_ threshold n k overlap smallCutoff = mkUGen Nothing [KR] (Left rate) "Tartini" [in_,threshold,n,k,overlap,smallCutoff] Nothing 2 (Special 0) NoId

-- | Neural Oscillator
--
--  TermanWang [AR] input=0.0 reset=0.0 ratex=1.0e-2 ratey=1.0e-2 alpha=1.0 beta=1.0 eta=1.0 initx=0.0 inity=0.0
termanWang :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
termanWang rate input reset ratex ratey alpha beta eta initx inity = mkUGen Nothing [AR] (Left rate) "TermanWang" [input,reset,ratex,ratey,alpha,beta,eta,initx,inity] Nothing 1 (Special 0) NoId

-- | display level of a UGen as a textual meter
--
--  TextVU [KR,AR] trig=2.0 in=0.0 label=0.0 width=21.0 reset=0.0 ana=0.0
textVU :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
textVU rate trig_ in_ label_ width reset ana = mkUGen Nothing [KR,AR] (Left rate) "TextVU" [trig_,in_,label_,width,reset,ana] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Tilt [AR] w=0.0 x=0.0 y=0.0 z=0.0 tilt=0.0
tilt :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tilt rate w x y z tilt_ = mkUGen Nothing [AR] (Left rate) "Tilt" [w,x,y,z,tilt_] Nothing 1 (Special 0) NoId

-- | triggered signal averager
--
--  TrigAvg [KR] in=0.0 trig=0.0
trigAvg :: Rate -> UGen -> UGen -> UGen
trigAvg rate in_ trig_ = mkUGen Nothing [KR] (Left rate) "TrigAvg" [in_,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Tumble [AR] w=0.0 x=0.0 y=0.0 z=0.0 tilt=0.0
tumble :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tumble rate w x y z tilt_ = mkUGen Nothing [AR] (Left rate) "Tumble" [w,x,y,z,tilt_] Nothing 1 (Special 0) NoId

-- | physical modeling simulation; two tubes
--
--  TwoTube [AR] input=0.0 k=1.0e-2 loss=1.0 d1length=100.0 d2length=100.0
twoTube :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
twoTube rate input k loss d1length d2length = mkUGen Nothing [AR] (Left rate) "TwoTube" [input,k,loss,d1length,d2length] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  UHJ2B [AR] ls=0.0 rs=0.0
uhj2b :: Rate -> UGen -> UGen -> UGen
uhj2b rate ls rs = mkUGen Nothing [AR] (Left rate) "UHJ2B" [ls,rs] Nothing 3 (Special 0) NoId

-- | Vector Base Amplitude Panner
--
--  VBAP [KR,AR] in=0.0 bufnum=0.0 azimuth=0.0 elevation=1.0 spread=0.0;    NC INPUT: True
vbap :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
vbap numChannels rate in_ bufnum azimuth elevation spread = mkUGen Nothing [KR,AR] (Left rate) "VBAP" [in_,bufnum,azimuth,elevation,spread] Nothing numChannels (Special 0) NoId

-- | a chaotic oscillator network
--
--  VBFourses [AR] smoother=0.5 *freqarray=0.0;    MCE=1, REORDERS INPUTS: [1,0]
vbFourses :: Rate -> UGen -> UGen -> UGen
vbFourses rate smoother freqarray = mkUGen Nothing [AR] (Left rate) "VBFourses" [smoother] (Just [freqarray]) 4 (Special 0) NoId

-- | artifical reverberator
--
--  VBJonVerb [AR] in=0.0 decay=0.6 damping=0.3 inputbw=0.8 erfl=0.5 tail=0.5;    FILTER: TRUE
vbJonVerb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
vbJonVerb in_ decay_ damping inputbw erfl tail_ = mkUGen Nothing [AR] (Right [0]) "VBJonVerb" [in_,decay_,damping,inputbw,erfl,tail_] Nothing 2 (Special 0) NoId

-- | a simple phase vocoder for time-stretching
--
--  VBPVoc [AR] numChannels=0.0 bufnum=0.0 playpos=0.0 fftsize=2048.0
vbpVoc :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vbpVoc rate numChannels bufnum playpos fftsize = mkUGen Nothing [AR] (Left rate) "VBPVoc" [numChannels,bufnum,playpos,fftsize] Nothing 1 (Special 0) NoId

-- | lowpass filter for envelope following
--
--  VBSlide [KR,AR] in=0.0 slideup=50.0 slidedown=3000.0;    FILTER: TRUE
vbSlide :: UGen -> UGen -> UGen -> UGen
vbSlide in_ slideup slidedown = mkUGen Nothing [KR,AR] (Right [0]) "VBSlide" [in_,slideup,slidedown] Nothing 1 (Special 0) NoId

-- | 2D scanning pattern virtual machine
--
--  VMScan2D [AR] bufnum=0.0
vmScan2D :: Rate -> UGen -> UGen
vmScan2D rate bufnum = mkUGen Nothing [AR] (Left rate) "VMScan2D" [bufnum] Nothing 2 (Special 0) NoId

-- | vosim pulse generator
--
--  VOSIM [AR] trig=0.1 freq=400.0 nCycles=1.0 decay=0.9
vosim :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vosim rate trig_ freq nCycles decay_ = mkUGen Nothing [AR] (Left rate) "VOSIM" [trig_,freq,nCycles,decay_] Nothing 1 (Special 0) NoId

-- | windowed amplitude follower
--
--  WAmp [KR] in=0.0 winSize=0.1
wAmp :: Rate -> UGen -> UGen -> UGen
wAmp rate in_ winSize = mkUGen Nothing [KR] (Left rate) "WAmp" [in_,winSize] Nothing 1 (Special 0) NoId

-- | decomposition into square waves, and reconstruction
--
--  WalshHadamard [AR] input=0.0 which=0.0
walshHadamard :: Rate -> UGen -> UGen -> UGen
walshHadamard rate input which = mkUGen Nothing [AR] (Left rate) "WalshHadamard" [input,which] Nothing 1 (Special 0) NoId

-- | Warp a buffer with a time pointer
--
--  WarpZ [AR] bufnum=0.0 pointer=0.0 freqScale=1.0 windowSize=0.2 envbufnum=-1.0 overlaps=8.0 windowRandRatio=0.0 interp=1.0 zeroSearch=0.0 zeroStart=0.0;    NC INPUT: True
warpZ :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
warpZ numChannels rate bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp zeroSearch zeroStart = mkUGen Nothing [AR] (Left rate) "WarpZ" [bufnum,pointer,freqScale,windowSize,envbufnum,overlaps,windowRandRatio,interp,zeroSearch,zeroStart] Nothing numChannels (Special 0) NoId

-- | Lose bits of your waves
--
--  WaveLoss [KR,AR] in=0.0 drop=20.0 outof=40.0 mode=1.0
waveLoss :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
waveLoss rate in_ drop_ outof mode = mkUGen Nothing [KR,AR] (Left rate) "WaveLoss" [in_,drop_,outof,mode] Nothing 1 (Special 0) NoId

-- | wave terrain synthesis
--
--  WaveTerrain [AR] bufnum=0.0 x=0.0 y=0.0 xsize=100.0 ysize=100.0
waveTerrain :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
waveTerrain rate bufnum x y xsize ysize = mkUGen Nothing [AR] (Left rate) "WaveTerrain" [bufnum,x,y,xsize,ysize] Nothing 1 (Special 0) NoId

-- | decomposition into Daub4 wavelets, and reconstruction
--
--  WaveletDaub [AR] input=0.0 n=64.0 which=0.0
waveletDaub :: Rate -> UGen -> UGen -> UGen -> UGen
waveletDaub rate input n which = mkUGen Nothing [AR] (Left rate) "WaveletDaub" [input,n,which] Nothing 1 (Special 0) NoId

-- | Weakly Nonlinear Oscillator
--
--  WeaklyNonlinear [AR] input=0.0 reset=0.0 ratex=1.0 ratey=1.0 freq=440.0 initx=0.0 inity=0.0 alpha=0.0 xexponent=0.0 beta=0.0 yexponent=0.0
weaklyNonlinear :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
weaklyNonlinear rate input reset ratex ratey freq initx inity alpha xexponent beta yexponent = mkUGen Nothing [AR] (Left rate) "WeaklyNonlinear" [input,reset,ratex,ratey,freq,initx,inity,alpha,xexponent,beta,yexponent] Nothing 1 (Special 0) NoId

-- | Weakly Nonlinear Oscillator
--
--  WeaklyNonlinear2 [AR] input=0.0 reset=0.0 ratex=1.0 ratey=1.0 freq=440.0 initx=0.0 inity=0.0 alpha=0.0 xexponent=0.0 beta=0.0 yexponent=0.0
weaklyNonlinear2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
weaklyNonlinear2 rate input reset ratex ratey freq initx inity alpha xexponent beta yexponent = mkUGen Nothing [AR] (Left rate) "WeaklyNonlinear2" [input,reset,ratex,ratey,freq,initx,inity,alpha,xexponent,beta,yexponent] Nothing 1 (Special 0) NoId

-- | Pulse counter with floating point steps
--
--  WrapSummer [KR,AR] trig=0.0 step=1.0 min=0.0 max=1.0 reset=0.0 resetval=0.0
wrapSummer :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
wrapSummer rate trig_ step min_ max_ reset resetval = mkUGen Nothing [KR,AR] (Left rate) "WrapSummer" [trig_,step,min_,max_,reset,resetval] Nothing 1 (Special 0) NoId
