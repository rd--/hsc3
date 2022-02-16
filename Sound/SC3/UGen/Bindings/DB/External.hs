-- | SC3 UGen bindings (auto-generated).
module Sound.SC3.UGen.Bindings.DB.External where

--import Sound.SC3.Common.Enum
--import Sound.SC3.Common.Envelope
import Sound.SC3.Common.Rate
import Sound.SC3.Common.UId
import Sound.SC3.Common.Unsafe
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | (Undocumented class)
--
--  A2B [AudioRate] a=0 b=0 c=0 d=0
a2b :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
a2b rate a b c d = mkUGen Nothing [AudioRate] (Left rate) "A2B" [a,b,c,d] Nothing 4 (Special 0) NoId

-- | Emulator of the AY (aka YM) soundchip, used in Spectrum/Atari
--
--  AY [AudioRate] tonea=1777 toneb=1666 tonec=1555 noise=1 control=7 vola=15 volb=15 volc=15 envfreq=4 envstyle=1 chiptype=0
ay :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
ay rate tonea toneb tonec noise control_ vola volb volc envfreq envstyle chiptype = mkUGen Nothing [AudioRate] (Left rate) "AY" [tonea,toneb,tonec,noise,control_,vola,volb,volc,envfreq,envstyle,chiptype] Nothing 1 (Special 0) NoId

-- | AY-3-891X Chip Sound Simulator
--
--  AY8910 [AudioRate] r0=0 r1=0 r2=0 r3=0 r4=0 r5=0 r6=0 r7=0 r8=0 r9=0 rA=0 rB=0 rC=0 rD=0 rate=1
ay8910 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
ay8910 rate r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 rA rB rC rD rate_ = mkUGen Nothing [AudioRate] (Left rate) "AY8910" [r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,rA,rB,rC,rD,rate_] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  Allpass1 [AudioRate] in=0 freq=1200
allpass1 :: Rate -> UGen -> UGen -> UGen
allpass1 rate in_ freq = mkUGen Nothing [AudioRate] (Left rate) "Allpass1" [in_,freq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Allpass2 [AudioRate] in=0 freq=1200 rq=1
allpass2 :: Rate -> UGen -> UGen -> UGen -> UGen
allpass2 rate in_ freq rq = mkUGen Nothing [AudioRate] (Left rate) "Allpass2" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | amplitude follower (deprecated)
--
--  AmplitudeMod [ControlRate,AudioRate] in=0 attackTime=0.01 releaseTime=0.01
amplitudeMod :: Rate -> UGen -> UGen -> UGen -> UGen
amplitudeMod rate in_ attackTime releaseTime = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "AmplitudeMod" [in_,attackTime,releaseTime] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogBassDrum [AudioRate] trig=0 infsustain=0 accent=0.5 freq=50 tone=0.5 decay=0.5 attackfm=0.5 selffm=0.25
analogBassDrum :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
analogBassDrum rate trig_ infsustain accent freq tone decay_ attackfm selffm = mkUGen Nothing [AudioRate] (Left rate) "AnalogBassDrum" [trig_,infsustain,accent,freq,tone,decay_,attackfm,selffm] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogFoldOsc [AudioRate] freq=100 amp=1
analogFoldOsc :: Rate -> UGen -> UGen -> UGen
analogFoldOsc rate freq amp = mkUGen Nothing [AudioRate] (Left rate) "AnalogFoldOsc" [freq,amp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogPhaser [AudioRate] input=0 lfoinput=0 skew=0 feedback=0.25 modulation=0.5 stages=8;    FILTER: TRUE
analogPhaser :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
analogPhaser input lfoinput skew feedback modulation stages = mkUGen Nothing [AudioRate] (Right [0]) "AnalogPhaser" [input,lfoinput,skew,feedback,modulation,stages] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogPhaserMod [ControlRate,AudioRate] input=0 skew=0 modulation=0.5 stages=8;    FILTER: TRUE
analogPhaserMod :: UGen -> UGen -> UGen -> UGen -> UGen
analogPhaserMod input skew modulation stages = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "AnalogPhaserMod" [input,skew,modulation,stages] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogPulseShaper [AudioRate] pulseinput=0 width=0.5 decay=0.5 double=0.5
analogPulseShaper :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
analogPulseShaper rate pulseinput width decay_ double = mkUGen Nothing [AudioRate] (Left rate) "AnalogPulseShaper" [pulseinput,width,decay_,double] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogSnareDrum [AudioRate] trig=0 infsustain=0 accent=0.1 freq=200 tone=0.5 decay=0.5 snappy=0.5
analogSnareDrum :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
analogSnareDrum rate trig_ infsustain accent freq tone decay_ snappy = mkUGen Nothing [AudioRate] (Left rate) "AnalogSnareDrum" [trig_,infsustain,accent,freq,tone,decay_,snappy] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogTape [AudioRate] input=0 bias=0.5 saturation=0.5 drive=0.5 oversample=1 mode=0;    FILTER: TRUE
analogTape :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
analogTape input bias saturation drive oversample mode = mkUGen Nothing [AudioRate] (Right [0]) "AnalogTape" [input,bias,saturation,drive,oversample,mode] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogVintageDistortion [AudioRate] input=0 drivegain=0.5 bias=0 lowgain=0.1 highgain=0.1 shelvingfreq=600 oversample=0;    FILTER: TRUE
analogVintageDistortion :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
analogVintageDistortion input drivegain bias lowgain highgain shelvingfreq oversample = mkUGen Nothing [AudioRate] (Right [0]) "AnalogVintageDistortion" [input,drivegain,bias,lowgain,highgain,shelvingfreq,oversample] Nothing 1 (Special 0) NoId

-- | event analyser (BBCut)
--
--  AnalyseEvents2 [AudioRate] in=0 bufnum=0 threshold=0.34 triggerid=101 circular=0 pitch=0
analyseEvents2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
analyseEvents2 rate in_ bufnum threshold triggerid circular pitch_ = mkUGen Nothing [AudioRate] (Left rate) "AnalyseEvents2" [in_,bufnum,threshold,triggerid,circular,pitch_] Nothing 1 (Special 0) NoId

-- | 2-species Predator-Prey model
--
--  ArneodoCoulletTresser [AudioRate] freq=22050 alpha=1.5 h=0.05 xi=0.5 yi=0.5 zi=0.5
arneodoCoulletTresser :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
arneodoCoulletTresser rate freq alpha h xi yi zi = mkUGen Nothing [AudioRate] (Left rate) "ArneodoCoulletTresser" [freq,alpha,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | detect the largest value (and its position) in an array of UGens
--
--  ArrayMax [ControlRate,AudioRate] *array=0;    MCE=1, FILTER: TRUE
arrayMax :: UGen -> UGen
arrayMax array = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "ArrayMax" [] (Just [array]) 2 (Special 0) NoId

-- | detect the smallest value (and its position) in an array of UGens
--
--  ArrayMin [ControlRate,AudioRate] *array=0;    MCE=1, FILTER: TRUE
arrayMin :: UGen -> UGen
arrayMin array = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "ArrayMin" [] (Just [array]) 2 (Special 0) NoId

-- | Sound Chip Simulator
--
--  Astrocade [AudioRate] reg0=0 reg1=127 reg2=0 reg3=0 reg4=0 reg5=0 reg6=15 reg7=0
astrocade :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
astrocade rate reg0 reg1 reg2 reg3 reg4 reg5 reg6 reg7 = mkUGen Nothing [AudioRate] (Left rate) "Astrocade" [reg0,reg1,reg2,reg3,reg4,reg5,reg6,reg7] Nothing 1 (Special 0) NoId

-- | TIA Chip Sound Simulator
--
--  Atari2600 [AudioRate] audc0=1 audc1=2 audf0=3 audf1=4 audv0=5 audv1=5 rate=1
atari2600 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atari2600 rate audc0 audc1 audf0 audf1 audv0 audv1 rate_ = mkUGen Nothing [AudioRate] (Left rate) "Atari2600" [audc0,audc1,audf0,audf1,audv0,audv1,rate_] Nothing 1 (Special 0) NoId

-- | Use Amp data from a given partial
--
--  AtsAmp [ControlRate,AudioRate] atsbuffer=0 partialNum=0 filePointer=0
atsAmp :: Rate -> UGen -> UGen -> UGen -> UGen
atsAmp rate atsbuffer partialNum filePointer = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "AtsAmp" [atsbuffer,partialNum,filePointer] Nothing 1 (Special 0) NoId

-- | (put short description here)
--
--  AtsBand [AudioRate] atsbuffer=0 band=0 filePointer=0
atsBand :: Rate -> UGen -> UGen -> UGen -> UGen
atsBand rate atsbuffer band filePointer = mkUGen Nothing [AudioRate] (Left rate) "AtsBand" [atsbuffer,band,filePointer] Nothing 1 (Special 0) NoId

-- | Use Freq data from a given partial
--
--  AtsFreq [ControlRate,AudioRate] atsbuffer=0 partialNum=0 filePointer=0
atsFreq :: Rate -> UGen -> UGen -> UGen -> UGen
atsFreq rate atsbuffer partialNum filePointer = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "AtsFreq" [atsbuffer,partialNum,filePointer] Nothing 1 (Special 0) NoId

-- | Resynthesize sine and noise data from an ATS analysis file
--
--  AtsNoiSynth [AudioRate] atsbuffer=0 numPartials=0 partialStart=0 partialSkip=1 filePointer=0 sinePct=1 noisePct=1 freqMul=1 freqAdd=0 numBands=25 bandStart=0 bandSkip=1
atsNoiSynth :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atsNoiSynth rate atsbuffer numPartials partialStart partialSkip filePointer sinePct noisePct freqMul freqAdd numBands bandStart bandSkip = mkUGen Nothing [AudioRate] (Left rate) "AtsNoiSynth" [atsbuffer,numPartials,partialStart,partialSkip,filePointer,sinePct,noisePct,freqMul,freqAdd,numBands,bandStart,bandSkip] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AtsNoise [ControlRate,AudioRate] atsbuffer=0 bandNum=0 filePointer=0
atsNoise :: Rate -> UGen -> UGen -> UGen -> UGen
atsNoise rate atsbuffer bandNum filePointer = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "AtsNoise" [atsbuffer,bandNum,filePointer] Nothing 1 (Special 0) NoId

-- | One UGen to return both Amp and Freq info
--
--  AtsParInfo [ControlRate,AudioRate] atsbuffer=0 partialNum=0 filePointer=0
atsParInfo :: Rate -> UGen -> UGen -> UGen -> UGen
atsParInfo rate atsbuffer partialNum filePointer = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "AtsParInfo" [atsbuffer,partialNum,filePointer] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  AtsPartial [AudioRate] atsbuffer=0 partial=0 filePointer=0 freqMul=1 freqAdd=0
atsPartial :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atsPartial rate atsbuffer partial filePointer freqMul freqAdd = mkUGen Nothing [AudioRate] (Left rate) "AtsPartial" [atsbuffer,partial,filePointer,freqMul,freqAdd] Nothing 1 (Special 0) NoId

-- | Resynthesize sine data from an ATS analysis file
--
--  AtsSynth [AudioRate] atsbuffer=0 numPartials=0 partialStart=0 partialSkip=1 filePointer=0 freqMul=1 freqAdd=0
atsSynth :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atsSynth rate atsbuffer numPartials partialStart partialSkip filePointer freqMul freqAdd = mkUGen Nothing [AudioRate] (Left rate) "AtsSynth" [atsbuffer,numPartials,partialStart,partialSkip,filePointer,freqMul,freqAdd] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AtsUGen [] maxSize=0
atsUGen :: Rate -> UGen -> UGen
atsUGen rate maxSize = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "AtsUGen" [maxSize] Nothing 1 (Special 0) NoId

-- | Detect onsets and assess the nature of the attack slope
--
--  AttackSlope [ControlRate] input=0 windowsize=1024 peakpicksize=20 leak=0.999 energythreshold=0.01 sumthreshold=20 mingap=30 numslopesaveraged=10
attackSlope :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
attackSlope rate input windowsize peakpicksize leak energythreshold sumthreshold mingap numslopesaveraged = mkUGen Nothing [ControlRate] (Left rate) "AttackSlope" [input,windowsize,peakpicksize,leak,energythreshold,sumthreshold,mingap,numslopesaveraged] Nothing 6 (Special 0) NoId

-- | (Undocumented class)
--
--  AudioMSG [AudioRate] in=0 index=0;    FILTER: TRUE
audioMSG :: UGen -> UGen -> UGen
audioMSG in_ index_ = mkUGen Nothing [AudioRate] (Right [0]) "AudioMSG" [in_,index_] Nothing 1 (Special 0) NoId

-- | calculates mean average of audio or control rate signal
--
--  AverageOutput [ControlRate,AudioRate] in=0 trig=0;    FILTER: TRUE
averageOutput :: UGen -> UGen -> UGen
averageOutput in_ trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "AverageOutput" [in_,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  B2A [AudioRate] w=0 x=0 y=0 z=0
b2a :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
b2a rate w x y z = mkUGen Nothing [AudioRate] (Left rate) "B2A" [w,x,y,z] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  B2Ster [AudioRate] w=0 x=0 y=0
b2Ster :: Rate -> UGen -> UGen -> UGen -> UGen
b2Ster rate w x y = mkUGen Nothing [AudioRate] (Left rate) "B2Ster" [w,x,y] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  B2UHJ [AudioRate] w=0 x=0 y=0
b2uhj :: Rate -> UGen -> UGen -> UGen -> UGen
b2uhj rate w x y = mkUGen Nothing [AudioRate] (Left rate) "B2UHJ" [w,x,y] Nothing 2 (Special 0) NoId

-- | MultiOut BetaBlocker VChip
--
--  BBlockerBuf [AudioRate] freq=0 bufnum=0 startpoint=0
bBlockerBuf :: Rate -> UGen -> UGen -> UGen -> UGen
bBlockerBuf rate freq bufnum startpoint = mkUGen Nothing [AudioRate] (Left rate) "BBlockerBuf" [freq,bufnum,startpoint] Nothing 9 (Special 0) NoId

-- | 3D Ambisonic decoder
--
--  BFDecode1 [AudioRate] w=0 x=0 y=0 z=0 azimuth=0 elevation=0 wComp=0
bfDecode1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bfDecode1 rate w x y z azimuth elevation wComp = mkUGen Nothing [AudioRate] (Left rate) "BFDecode1" [w,x,y,z,azimuth,elevation,wComp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BFDecoder [] maxSize=0
bfDecoder :: Rate -> UGen -> UGen
bfDecoder rate maxSize = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "BFDecoder" [maxSize] Nothing 1 (Special 0) NoId

-- | Ambisonic B format encoder
--
--  BFEncode1 [AudioRate] in=0 azimuth=0 elevation=0 rho=1 gain=1 wComp=0
bfEncode1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bfEncode1 rate in_ azimuth elevation rho gain wComp = mkUGen Nothing [AudioRate] (Left rate) "BFEncode1" [in_,azimuth,elevation,rho,gain,wComp] Nothing 4 (Special 0) NoId

-- | Ambisonic B format encoder
--
--  BFEncode2 [AudioRate] in=0 point_x=1 point_y=1 elevation=0 gain=1 wComp=0
bfEncode2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bfEncode2 rate in_ point_x point_y elevation gain wComp = mkUGen Nothing [AudioRate] (Left rate) "BFEncode2" [in_,point_x,point_y,elevation,gain,wComp] Nothing 4 (Special 0) NoId

-- | Ambisonic B format encoder for stereo signals
--
--  BFEncodeSter [AudioRate] l=0 r=0 azimuth=0 width=1.5708 elevation=0 rho=1 gain=1 wComp=0
bfEncodeSter :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bfEncodeSter rate l r azimuth width elevation rho gain wComp = mkUGen Nothing [AudioRate] (Left rate) "BFEncodeSter" [l,r,azimuth,width,elevation,rho,gain,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  BFGrainPanner [] maxSize=0
bfGrainPanner :: Rate -> UGen -> UGen
bfGrainPanner rate maxSize = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "BFGrainPanner" [maxSize] Nothing 1 (Special 0) NoId

-- | BFormat sound manipulation
--
--  BFManipulate [AudioRate] w=0 x=0 y=0 z=0 rotate=0 tilt=0 tumble=0
bfManipulate :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bfManipulate rate w x y z rotate_ tilt_ tumble_ = mkUGen Nothing [AudioRate] (Left rate) "BFManipulate" [w,x,y,z,rotate_,tilt_,tumble_] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  BFPanner [] maxSize=0
bfPanner :: Rate -> UGen -> UGen
bfPanner rate maxSize = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "BFPanner" [maxSize] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BLBufRd [ControlRate,AudioRate] bufnum=0 phase=0 ratio=1
blBufRd :: Rate -> UGen -> UGen -> UGen -> UGen
blBufRd rate bufnum phase ratio = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "BLBufRd" [bufnum,phase,ratio] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BLOsc [ControlRate,AudioRate] freq=100 pulsewidth=0.5 waveform=0
blOsc :: Rate -> UGen -> UGen -> UGen -> UGen
blOsc rate freq pulsewidth waveform = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "BLOsc" [freq,pulsewidth,waveform] Nothing 1 (Special 0) NoId

-- | 24db/oct rolloff - 4nd order resonant Low/High/Band Pass Filter
--
--  BMoog [AudioRate] in=0 freq=440 q=0.2 mode=0 saturation=0.95;    FILTER: TRUE
bMoog :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bMoog in_ freq q mode saturation = mkUGen Nothing [AudioRate] (Right [0]) "BMoog" [in_,freq,q,mode,saturation] Nothing 1 (Special 0) NoId

-- | Balances two signals with each other
--
--  Balance [AudioRate] in=0 test=0 hp=10 stor=0
balance :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
balance rate in_ test hp stor = mkUGen Nothing [AudioRate] (Left rate) "Balance" [in_,test,hp,stor] Nothing 1 (Special 0) NoId

-- | Extracts statistics on a beat histogram
--
--  BeatStatistics [ControlRate] fft=0 leak=0.995 numpreviousbeats=4
beatStatistics :: Rate -> UGen -> UGen -> UGen -> UGen
beatStatistics rate fft_ leak numpreviousbeats = mkUGen Nothing [ControlRate] (Left rate) "BeatStatistics" [fft_,leak,numpreviousbeats] Nothing 4 (Special 0) NoId

-- | Sound Chip Simulator (well...)
--
--  Beep [AudioRate] freq=3250 vol=1
beep :: Rate -> UGen -> UGen -> UGen
beep rate freq vol = mkUGen Nothing [AudioRate] (Left rate) "Beep" [freq,vol] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BeepU [] maxSize=0
beepU ::  Rate -> UGen -> UGen
beepU rate maxSize = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "BeepU" [maxSize] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BinData [ControlRate,AudioRate] buffer=0 bin=0 overlaps=0.5
binData :: Rate -> UGen -> UGen -> UGen -> UGen
binData rate buffer bin overlaps = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "BinData" [buffer,bin,overlaps] Nothing 2 (Special 0) NoId

-- | Band limited impulse generation
--
--  BlitB3 [AudioRate] freq=440
blitB3 :: Rate -> UGen -> UGen
blitB3 rate freq = mkUGen Nothing [AudioRate] (Left rate) "BlitB3" [freq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BlitB3D [AudioRate] freq=440
blitB3D :: Rate -> UGen -> UGen
blitB3D rate freq = mkUGen Nothing [AudioRate] (Left rate) "BlitB3D" [freq] Nothing 1 (Special 0) NoId

-- | BLIT derived sawtooth
--
--  BlitB3Saw [AudioRate] freq=440 leak=0.99
blitB3Saw :: Rate -> UGen -> UGen -> UGen
blitB3Saw rate freq leak = mkUGen Nothing [AudioRate] (Left rate) "BlitB3Saw" [freq,leak] Nothing 1 (Special 0) NoId

-- | Bipolar BLIT derived square waveform
--
--  BlitB3Square [AudioRate] freq=440 leak=0.99
blitB3Square :: Rate -> UGen -> UGen -> UGen
blitB3Square rate freq leak = mkUGen Nothing [AudioRate] (Left rate) "BlitB3Square" [freq,leak] Nothing 1 (Special 0) NoId

-- | Bipolar BLIT derived triangle
--
--  BlitB3Tri [AudioRate] freq=440 leak=0.99 leak2=0.99
blitB3Tri :: Rate -> UGen -> UGen -> UGen -> UGen
blitB3Tri rate freq leak leak2 = mkUGen Nothing [AudioRate] (Left rate) "BlitB3Tri" [freq,leak,leak2] Nothing 1 (Special 0) NoId

-- | breakcore simulator
--
--  Breakcore [AudioRate] bufnum=0 capturein=0 capturetrigger=0 duration=0.1 ampdropout=0
breakcore :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
breakcore rate bufnum capturein capturetrigger duration ampdropout = mkUGen Nothing [AudioRate] (Left rate) "Breakcore" [bufnum,capturein,capturetrigger,duration,ampdropout] Nothing 1 (Special 0) NoId

-- | Prigogine oscillator
--
--  Brusselator [AudioRate] reset=0 rate=0.01 mu=1 gamma=1 initx=0.5 inity=0.5
brusselator :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
brusselator rate reset rate_ mu gamma initx inity = mkUGen Nothing [AudioRate] (Left rate) "Brusselator" [reset,rate_,mu,gamma,initx,inity] Nothing 2 (Special 0) NoId

-- | Granular synthesis with sound sampled in a buffer
--
--  BufGrain [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 interp=2
bufGrain :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrain rate trigger dur sndbuf rate_ pos interp = mkUGen Nothing [AudioRate] (Left rate) "BufGrain" [trigger,dur,sndbuf,rate_,pos,interp] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sound sampled in a buffer and user supplied envelope
--
--  BufGrainB [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 envbuf=0 interp=2
bufGrainB :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrainB rate trigger dur sndbuf rate_ pos envbuf interp = mkUGen Nothing [AudioRate] (Left rate) "BufGrainB" [trigger,dur,sndbuf,rate_,pos,envbuf,interp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BufGrainBBF [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 envbuf=0 azimuth=0 elevation=0 rho=1 interp=2 wComp=0
bufGrainBBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrainBBF rate trigger dur sndbuf rate_ pos envbuf azimuth elevation rho interp wComp = mkUGen Nothing [AudioRate] (Left rate) "BufGrainBBF" [trigger,dur,sndbuf,rate_,pos,envbuf,azimuth,elevation,rho,interp,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  BufGrainBF [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 azimuth=0 elevation=0 rho=1 interp=2 wComp=0
bufGrainBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrainBF rate trigger dur sndbuf rate_ pos azimuth elevation rho interp wComp = mkUGen Nothing [AudioRate] (Left rate) "BufGrainBF" [trigger,dur,sndbuf,rate_,pos,azimuth,elevation,rho,interp,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with sound sampled in a buffer and user supplied envelopes
--
--  BufGrainI [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 envbuf1=0 envbuf2=0 ifac=0.5 interp=2
bufGrainI :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrainI rate trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac interp = mkUGen Nothing [AudioRate] (Left rate) "BufGrainI" [trigger,dur,sndbuf,rate_,pos,envbuf1,envbuf2,ifac,interp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BufGrainIBF [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 envbuf1=0 envbuf2=0 ifac=0.5 azimuth=0 elevation=0 rho=1 interp=2 wComp=0
bufGrainIBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bufGrainIBF rate trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac azimuth elevation rho interp wComp = mkUGen Nothing [AudioRate] (Left rate) "BufGrainIBF" [trigger,dur,sndbuf,rate_,pos,envbuf1,envbuf2,ifac,azimuth,elevation,rho,interp,wComp] Nothing 4 (Special 0) NoId

-- | detect the largest value (and its position) in an array of UGens
--
--  BufMax [ControlRate] bufnum=0 gate=1
bufMax :: Rate -> UGen -> UGen -> UGen
bufMax rate bufnum gate_ = mkUGen Nothing [ControlRate] (Left rate) "BufMax" [bufnum,gate_] Nothing 2 (Special 0) NoId

-- | detect the largest value (and its position) in an array of UGens
--
--  BufMin [ControlRate] bufnum=0 gate=1
bufMin :: Rate -> UGen -> UGen -> UGen
bufMin rate bufnum gate_ = mkUGen Nothing [ControlRate] (Left rate) "BufMin" [bufnum,gate_] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  CQ_Diff [ControlRate] in1=0 in2=0 databufnum=0
cq_Diff :: Rate -> UGen -> UGen -> UGen -> UGen
cq_Diff rate in1 in2 databufnum = mkUGen Nothing [ControlRate] (Left rate) "CQ_Diff" [in1,in2,databufnum] Nothing 1 (Special 0) NoId

-- | Quefrency analysis and liftering
--
--  Cepstrum [] cepbuf=0 fftchain=0
cepstrum :: Rate -> UGen -> UGen -> UGen
cepstrum rate cepbuf fftchain = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "Cepstrum" [cepbuf,fftchain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Chen [ControlRate,AudioRate] speed=0.5 a=0.5 b=0.3 c=0.28
chen :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
chen rate speed a b c = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Chen" [speed,a,b,c] Nothing 3 (Special 0) NoId

-- | Octave chroma band based representation of energy in a signal; Chromagram for nTET tuning systems with any base reference
--
--  Chromagram [ControlRate] fft=0 fftsize=2048 n=12 tuningbase=32.7032 octaves=8 integrationflag=0 coeff=0.9 octaveratio=2 perframenormalize=0
chromagram :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
chromagram rate fft_ fftsize n tuningbase octaves integrationflag coeff octaveratio perframenormalize = mkUGen Nothing [ControlRate] (Left rate) "Chromagram" [fft_,fftsize,n,tuningbase,octaves,integrationflag,coeff,octaveratio,perframenormalize] Nothing 12 (Special 0) NoId

-- | circular linear lag
--
--  CircleRamp [ControlRate,AudioRate] in=0 lagTime=0.1 circmin=-180 circmax=180
circleRamp :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
circleRamp rate in_ lagTime circmin circmax = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "CircleRamp" [in_,lagTime,circmin,circmax] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clipper32 [AudioRate] in=0 lo=-0.8 hi=0.8
clipper32 :: Rate -> UGen -> UGen -> UGen -> UGen
clipper32 rate in_ lo hi = mkUGen Nothing [AudioRate] (Left rate) "Clipper32" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clipper4 [AudioRate] in=0 lo=-0.8 hi=0.8
clipper4 :: Rate -> UGen -> UGen -> UGen -> UGen
clipper4 rate in_ lo hi = mkUGen Nothing [AudioRate] (Left rate) "Clipper4" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clipper8 [AudioRate] in=0 lo=-0.8 hi=0.8
clipper8 :: Rate -> UGen -> UGen -> UGen -> UGen
clipper8 rate in_ lo hi = mkUGen Nothing [AudioRate] (Left rate) "Clipper8" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clockmus [ControlRate]
clockmus :: Rate -> UGen
clockmus rate = mkUGen Nothing [ControlRate] (Left rate) "Clockmus" [] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  CombLP [AudioRate] in=0 gate=1 maxdelaytime=0.2 delaytime=0.2 decaytime=1 coef=0.5
combLP :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
combLP rate in_ gate_ maxdelaytime delaytime decaytime coef = mkUGen Nothing [AudioRate] (Left rate) "CombLP" [in_,gate_,maxdelaytime,delaytime,decaytime,coef] Nothing 1 (Special 0) NoId

-- | FM-modulable resonating filter
--
--  ComplexRes [AudioRate] in=0 freq=100 decay=0.2;    FILTER: TRUE
complexRes :: UGen -> UGen -> UGen -> UGen
complexRes in_ freq decay_ = mkUGen Nothing [AudioRate] (Right [0]) "ComplexRes" [in_,freq,decay_] Nothing 1 (Special 0) NoId

-- | Concatenative Cross-Synthesis on Live Streams
--
--  Concat [AudioRate] control=0 source=0 storesize=1 seektime=1 seekdur=1 matchlength=0.05 freezestore=0 zcr=1 lms=1 sc=1 st=0 randscore=0
concat :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
concat rate control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore = mkUGen Nothing [AudioRate] (Left rate) "Concat" [control_,source,storesize,seektime,seekdur,matchlength,freezestore,zcr,lms,sc,st,randscore] Nothing 1 (Special 0) NoId

-- | Concatenative Cross-Synthesis on Live Streams
--
--  Concat2 [AudioRate] control=0 source=0 storesize=1 seektime=1 seekdur=1 matchlength=0.05 freezestore=0 zcr=1 lms=1 sc=1 st=0 randscore=0 threshold=0.01
concat2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
concat2 rate control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore threshold = mkUGen Nothing [AudioRate] (Left rate) "Concat2" [control_,source,storesize,seektime,seekdur,matchlength,freezestore,zcr,lms,sc,st,randscore,threshold] Nothing 1 (Special 0) NoId

-- | an amplitude tracking based onset detector
--
--  Coyote [ControlRate] in=0 trackFall=0.2 slowLag=0.2 fastLag=0.01 fastMul=0.5 thresh=0.05 minDur=0.1
coyote :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
coyote rate in_ trackFall slowLag fastLag fastMul thresh minDur = mkUGen Nothing [ControlRate] (Left rate) "Coyote" [in_,trackFall,slowLag,fastLag,fastMul,thresh,minDur] Nothing 1 (Special 0) NoId

-- | Measure the temporal crest factor of a signal
--
--  Crest [ControlRate] in=0 numsamps=400 gate=1
crest :: Rate -> UGen -> UGen -> UGen -> UGen
crest rate in_ numsamps gate_ = mkUGen Nothing [ControlRate] (Left rate) "Crest" [in_,numsamps,gate_] Nothing 1 (Special 0) NoId

-- | class B/AB power amp distortion simulation
--
--  CrossoverDistortion [AudioRate] in=0 amp=0.5 smooth=0.5;    FILTER: TRUE
crossoverDistortion :: UGen -> UGen -> UGen -> UGen
crossoverDistortion in_ amp smooth = mkUGen Nothing [AudioRate] (Right [0]) "CrossoverDistortion" [in_,amp,smooth] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  DCompressor [AudioRate] input=0 sidechainIn=0 sidechain=0 ratio=4 threshold=-40 attack=0.1 release=100.1 makeup=0.5 automakeup=1;    FILTER: TRUE
dCompressor :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dCompressor input sidechainIn sidechain ratio threshold attack release makeup automakeup = mkUGen Nothing [AudioRate] (Right [0]) "DCompressor" [input,sidechainIn,sidechain,ratio,threshold,attack,release,makeup,automakeup] Nothing 1 (Special 0) NoId

-- | Digitally modelled analog filter
--
--  DFM1 [AudioRate] in=0 freq=1000 res=0.1 inputgain=1 type=0 noiselevel=0.0003;    FILTER: TRUE
dfm1 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dfm1 in_ freq res inputgain type_ noiselevel = mkUGen Nothing [AudioRate] (Right [0]) "DFM1" [in_,freq,res,inputgain,type_,noiselevel] Nothing 1 (Special 0) NoId

-- | Demand rate implementation of a Wiard noise ring
--
--  DNoiseRing [DemandRate] change=0.5 chance=0.5 shift=1 numBits=8 resetval=0;    DEMAND/NONDET
dNoiseRingId :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dNoiseRingId z change chance shift numBits resetval = mkUGen Nothing [DemandRate] (Left DemandRate) "DNoiseRing" [change,chance,shift,numBits,resetval] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of DNoiseRing.
dNoiseRingM :: UId m => UGen -> UGen -> UGen -> UGen -> UGen -> m UGen
dNoiseRingM = liftUId5 dNoiseRingId

-- | Unsafe variant of DNoiseRing.
dNoiseRing ::  UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dNoiseRing = liftUnsafe5 dNoiseRingM

-- | Triangle via 3rd order differerentiated polynomial waveform
--
--  DPW3Tri [AudioRate] freq=440
dpw3Tri :: Rate -> UGen -> UGen
dpw3Tri rate freq = mkUGen Nothing [AudioRate] (Left rate) "DPW3Tri" [freq] Nothing 1 (Special 0) NoId

-- | Sawtooth via 4th order differerentiated polynomial waveform
--
--  DPW4Saw [AudioRate] freq=440
dpw4Saw :: Rate -> UGen -> UGen
dpw4Saw rate freq = mkUGen Nothing [AudioRate] (Left rate) "DPW4Saw" [freq] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGBowed [AudioRate] freq=440 velb=0.5 force=1 gate=1 pos=0.14 release=0.1 c1=1 c3=3 impZ=0.55 fB=2
dwgBowed :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgBowed rate freq velb force gate_ pos release c1 c3 impZ fB = mkUGen Nothing [AudioRate] (Left rate) "DWGBowed" [freq,velb,force,gate_,pos,release,c1,c3,impZ,fB] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGBowedSimple [AudioRate] freq=440 velb=0.5 force=1 gate=1 pos=0.14 release=0.1 c1=1 c3=30
dwgBowedSimple :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgBowedSimple rate freq velb force gate_ pos release c1 c3 = mkUGen Nothing [AudioRate] (Left rate) "DWGBowedSimple" [freq,velb,force,gate_,pos,release,c1,c3] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGBowedTor [AudioRate] freq=440 velb=0.5 force=1 gate=1 pos=0.14 release=0.1 c1=1 c3=3 impZ=0.55 fB=2 mistune=5.2 c1tor=1 c3tor=3000 iZtor=1.8
dwgBowedTor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgBowedTor rate freq velb force gate_ pos release c1 c3 impZ fB mistune c1tor c3tor iZtor = mkUGen Nothing [AudioRate] (Left rate) "DWGBowedTor" [freq,velb,force,gate_,pos,release,c1,c3,impZ,fB,mistune,c1tor,c3tor,iZtor] Nothing 1 (Special 0) NoId

-- | Clarinet physical model.
--
--  DWGClarinet3 [AudioRate] freq=440.0 pm=1.0 pc=1.0 m=0.8 gate=1.0 release=1.0e-2 c1=0.25 c3=7.0
dwgClarinet3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgClarinet3 rate freq pm pc m gate_ release c1 c3 = mkUGen Nothing [AudioRate] (Left rate) "DWGClarinet3" [freq,pm,pc,m,gate_,release,c1,c3] Nothing 1 (Special 0) NoId

-- | Reimplementation of STK flute model.
--
--  DWGFlute [AudioRate] freq=400.0 pm=1.0 endr=0.5 jetr=0.25 jetRa=0.33 gate=1.0 release=0.1
dwgFlute :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgFlute rate freq pm endr jetr jetRa gate_ release = mkUGen Nothing [AudioRate] (Left rate) "DWGFlute" [freq,pm,endr,jetr,jetRa,gate_,release] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGPlucked [AudioRate] freq=440 amp=0.5 gate=1 pos=0.14 c1=1 c3=30 inp=0 release=0.1
dwgPlucked :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgPlucked rate freq amp gate_ pos c1 c3 inp release = mkUGen Nothing [AudioRate] (Left rate) "DWGPlucked" [freq,amp,gate_,pos,c1,c3,inp,release] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGPlucked2 [AudioRate] freq=440 amp=0.5 gate=1 pos=0.14 c1=1 c3=30 inp=0 release=0.1 mistune=1.008 mp=0.55 gc=0.01
dwgPlucked2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgPlucked2 rate freq amp gate_ pos c1 c3 inp release mistune mp gc = mkUGen Nothing [AudioRate] (Left rate) "DWGPlucked2" [freq,amp,gate_,pos,c1,c3,inp,release,mistune,mp,gc] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGPluckedStiff [AudioRate] freq=440 amp=0.5 gate=1 pos=0.14 c1=1 c3=30 inp=0 release=0.1 fB=2
dwgPluckedStiff :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgPluckedStiff rate freq amp gate_ pos c1 c3 inp release fB = mkUGen Nothing [AudioRate] (Left rate) "DWGPluckedStiff" [freq,amp,gate_,pos,c1,c3,inp,release,fB] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  DWGSoundBoard [AudioRate] inp=0 c1=20 c3=20 mix=0.8 d1=199 d2=211 d3=223 d4=227 d5=229 d6=233 d7=239 d8=241;    FILTER: TRUE
dwgSoundBoard :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwgSoundBoard inp c1 c3 mix d1 d2 d3 d4 d5 d6 d7 d8 = mkUGen Nothing [AudioRate] (Right [0]) "DWGSoundBoard" [inp,c1,c3,mix,d1,d2,d3,d4,d5,d6,d7,d8] Nothing 1 (Special 0) NoId

-- | demand rate brownian movement with Gendyn distributions
--
--  Dbrown2 [] lo=0 hi=0 step=0 dist=0 length=100000000
dbrown2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dbrown2 rate lo hi step dist length_ = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "Dbrown2" [lo,hi,step,dist,length_] Nothing 1 (Special 0) NoId

-- | demand rate tag system on a buffer
--
--  DbufTag [DemandRate] bufnum=0 v=0 axiom=0 rules=0 recycle=0 mode=0;    DEMAND/NONDET
dbufTagId :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dbufTagId z bufnum v axiom rules recycle mode = mkUGen Nothing [DemandRate] (Left DemandRate) "DbufTag" [bufnum,v,axiom,rules,recycle,mode] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of DbufTag.
dbufTagM :: UId m => UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> m UGen
dbufTagM = liftUId6 dbufTagId

-- | Unsafe variant of DbufTag.
dbufTag ::  UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dbufTag = liftUnsafe6 dbufTagM

-- | Samplerate and bitrate reduction
--
--  Decimator [AudioRate] in=0 rate=44100 bits=24
decimator :: Rate -> UGen -> UGen -> UGen -> UGen
decimator rate in_ rate_ bits = mkUGen Nothing [AudioRate] (Left rate) "Decimator" [in_,rate_,bits] Nothing 1 (Special 0) NoId

-- | Demand version of the BetaBlocker VChip
--
--  DetaBlockerBuf [DemandRate] bufnum=0 startpoint=0;    DEMAND/NONDET
detaBlockerBufId :: ID a => a -> UGen -> UGen -> UGen
detaBlockerBufId z bufnum startpoint = mkUGen Nothing [DemandRate] (Left DemandRate) "DetaBlockerBuf" [bufnum,startpoint] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of DetaBlockerBuf.
detaBlockerBufM :: UId m => UGen -> UGen -> m UGen
detaBlockerBufM = liftUId2 detaBlockerBufId

-- | Unsafe variant of DetaBlockerBuf.
detaBlockerBuf ::  UGen -> UGen -> UGen
detaBlockerBuf = liftUnsafe2 detaBlockerBufM

-- | demand rate finite state machine
--
--  Dfsm [DemandRate] rules=0 n=1 rgen=0;    DEMAND/NONDET
dfsmId :: ID a => a -> UGen -> UGen -> UGen -> UGen
dfsmId z rules n rgen = mkUGen Nothing [DemandRate] (Left DemandRate) "Dfsm" [rules,n,rgen] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Dfsm.
dfsmM :: UId m => UGen -> UGen -> UGen -> m UGen
dfsmM = liftUId3 dfsmId

-- | Unsafe variant of Dfsm.
dfsm ::  UGen -> UGen -> UGen -> UGen
dfsm = liftUnsafe3 dfsmM

-- | (Undocumented class)
--
--  Dgauss [] lo=0 hi=0 length=100000000
dgauss :: Rate -> UGen -> UGen -> UGen -> UGen
dgauss rate lo hi length_ = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "Dgauss" [lo,hi,length_] Nothing 1 (Special 0) NoId

-- | Ring modulation based on the physical model of a diode.
--
--  DiodeRingMod [AudioRate] car=0 mod=0;    FILTER: TRUE
diodeRingMod :: UGen -> UGen -> UGen
diodeRingMod car mod_ = mkUGen Nothing [AudioRate] (Right [0]) "DiodeRingMod" [car,mod_] Nothing 1 (Special 0) NoId

-- | port of some ladspa plugins
--
--  Disintegrator [AudioRate] in=0 probability=0.5 multiplier=0;    FILTER: TRUE, NONDET
disintegratorId :: ID a => a -> UGen -> UGen -> UGen -> UGen
disintegratorId z in_ probability multiplier = mkUGen Nothing [AudioRate] (Right [0]) "Disintegrator" [in_,probability,multiplier] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of Disintegrator.
disintegratorM :: UId m => UGen -> UGen -> UGen -> m UGen
disintegratorM = liftUId3 disintegratorId

-- | Unsafe variant of Disintegrator.
disintegrator ::  UGen -> UGen -> UGen -> UGen
disintegrator = liftUnsafe3 disintegratorM

-- | discrete time neurodynamics
--
--  Dneuromodule [ControlRate,AudioRate,DemandRate] dt=0 *theta=0 *x=0 *weights=0;    MCE=3, NC INPUT: True, NONDET
dneuromoduleId :: ID a => Int -> a -> UGen -> UGen -> UGen -> UGen -> UGen
dneuromoduleId numChannels z dt theta x weights = mkUGen Nothing [ControlRate,AudioRate,DemandRate] (Left DemandRate) "Dneuromodule" [dt] (Just [theta,x,weights]) numChannels (Special 0) (toUId z)

-- | Monad variant of Dneuromodule.
dneuromoduleM :: UId m => Int -> UGen -> UGen -> UGen -> UGen -> m UGen
dneuromoduleM = liftUId5 dneuromoduleId

-- | Unsafe variant of Dneuromodule.
dneuromodule ::  Int -> UGen -> UGen -> UGen -> UGen -> UGen
dneuromodule = liftUnsafe5 dneuromoduleM

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  DoubleNestedAllpassC [AudioRate] in=0 maxdelay1=0.0047 delay1=0.0047 gain1=0.15 maxdelay2=0.022 delay2=0.022 gain2=0.25 maxdelay3=0.0083 delay3=0.0083 gain3=0.3;    FILTER: TRUE
doubleNestedAllpassC :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleNestedAllpassC in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 maxdelay3 delay3 gain3 = mkUGen Nothing [AudioRate] (Right [0]) "DoubleNestedAllpassC" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2,maxdelay3,delay3,gain3] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  DoubleNestedAllpassL [AudioRate] in=0 maxdelay1=0.0047 delay1=0.0047 gain1=0.15 maxdelay2=0.022 delay2=0.022 gain2=0.25 maxdelay3=0.0083 delay3=0.0083 gain3=0.3;    FILTER: TRUE
doubleNestedAllpassL :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleNestedAllpassL in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 maxdelay3 delay3 gain3 = mkUGen Nothing [AudioRate] (Right [0]) "DoubleNestedAllpassL" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2,maxdelay3,delay3,gain3] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  DoubleNestedAllpassN [AudioRate] in=0 maxdelay1=0.0047 delay1=0.0047 gain1=0.15 maxdelay2=0.022 delay2=0.022 gain2=0.25 maxdelay3=0.0083 delay3=0.0083 gain3=0.3;    FILTER: TRUE
doubleNestedAllpassN :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleNestedAllpassN in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 maxdelay3 delay3 gain3 = mkUGen Nothing [AudioRate] (Right [0]) "DoubleNestedAllpassN" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2,maxdelay3,delay3,gain3] Nothing 1 (Special 0) NoId

-- | Forced DoubleWell Oscillator
--
--  DoubleWell [AudioRate] reset=0 ratex=0.01 ratey=0.01 f=1 w=0.001 delta=1 initx=0 inity=0
doubleWell :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleWell rate reset ratex ratey f w delta initx inity = mkUGen Nothing [AudioRate] (Left rate) "DoubleWell" [reset,ratex,ratey,f,w,delta,initx,inity] Nothing 1 (Special 0) NoId

-- | Forced DoubleWell Oscillator
--
--  DoubleWell2 [AudioRate] reset=0 ratex=0.01 ratey=0.01 f=1 w=0.001 delta=1 initx=0 inity=0
doubleWell2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleWell2 rate reset ratex ratey f w delta initx inity = mkUGen Nothing [AudioRate] (Left rate) "DoubleWell2" [reset,ratex,ratey,f,w,delta,initx,inity] Nothing 1 (Special 0) NoId

-- | Forced DoubleWell Oscillator
--
--  DoubleWell3 [AudioRate] reset=0 rate=0.01 f=0 delta=0.25 initx=0 inity=0
doubleWell3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
doubleWell3 rate reset rate_ f delta initx inity = mkUGen Nothing [AudioRate] (Left rate) "DoubleWell3" [reset,rate_,f,delta,initx,inity] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  DriveNoise [AudioRate] in=0 amount=1 multi=5
driveNoise :: Rate -> UGen -> UGen -> UGen -> UGen
driveNoise rate in_ amount multi = mkUGen Nothing [AudioRate] (Left rate) "DriveNoise" [in_,amount,multi] Nothing 1 (Special 0) NoId

-- | Crosscorrelation search and drum pattern matching beat tracker
--
--  DrumTrack [ControlRate] in=0 lock=0 dynleak=0 tempowt=0 phasewt=0 basswt=0 patternwt=1 prior=0 kicksensitivity=1 snaresensitivity=1 debugmode=0
drumTrack :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
drumTrack rate in_ lock dynleak tempowt phasewt basswt patternwt prior kicksensitivity snaresensitivity debugmode = mkUGen Nothing [ControlRate] (Left rate) "DrumTrack" [in_,lock,dynleak,tempowt,phasewt,basswt,patternwt,prior,kicksensitivity,snaresensitivity,debugmode] Nothing 4 (Special 0) NoId

-- | demand rate tag system
--
--  Dtag [] bufsize=0 v=0 axiom=0 rules=0 recycle=0 mode=0
dtag :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dtag rate bufsize v axiom rules recycle mode = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "Dtag" [bufsize,v,axiom,rules,recycle,mode] Nothing 1 (Special 0) NoId

-- | Envelope Follower Filter
--
--  EnvDetect [AudioRate] in=0 attack=100 release=0
envDetect :: Rate -> UGen -> UGen -> UGen -> UGen
envDetect rate in_ attack release = mkUGen Nothing [AudioRate] (Left rate) "EnvDetect" [in_,attack,release] Nothing 1 (Special 0) NoId

-- | Envelope Follower
--
--  EnvFollow [ControlRate,AudioRate] input=0 decaycoeff=0.99
envFollow :: Rate -> UGen -> UGen -> UGen
envFollow rate input decaycoeff = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "EnvFollow" [input,decaycoeff] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTComplexDev [ControlRate] buffer=0 rectify=0 powthresh=0.1
fftComplexDev :: Rate -> UGen -> UGen -> UGen -> UGen
fftComplexDev rate buffer rectify powthresh = mkUGen Nothing [ControlRate] (Left rate) "FFTComplexDev" [buffer,rectify,powthresh] Nothing 1 (Special 0) NoId

-- | Spectral crest measure
--
--  FFTCrest [ControlRate] buffer=0 freqlo=0 freqhi=50000
fftCrest :: Rate -> UGen -> UGen -> UGen -> UGen
fftCrest rate buffer freqlo freqhi = mkUGen Nothing [ControlRate] (Left rate) "FFTCrest" [buffer,freqlo,freqhi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTDiffMags [ControlRate] bufferA=0 bufferB=0
fftDiffMags :: Rate -> UGen -> UGen -> UGen
fftDiffMags rate bufferA bufferB = mkUGen Nothing [ControlRate] (Left rate) "FFTDiffMags" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTFlux [ControlRate] buffer=0 normalise=1
fftFlux :: Rate -> UGen -> UGen -> UGen
fftFlux rate buffer normalise = mkUGen Nothing [ControlRate] (Left rate) "FFTFlux" [buffer,normalise] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTFluxPos [ControlRate] buffer=0 normalise=1
fftFluxPos :: Rate -> UGen -> UGen -> UGen
fftFluxPos rate buffer normalise = mkUGen Nothing [ControlRate] (Left rate) "FFTFluxPos" [buffer,normalise] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTMKL [ControlRate] buffer=0 epsilon=0.0
fftmkl :: Rate -> UGen -> UGen -> UGen
fftmkl rate buffer epsilon = mkUGen Nothing [ControlRate] (Left rate) "FFTMKL" [buffer,epsilon] Nothing 1 (Special 0) NoId

-- | Find peak value in an FFT frame
--
--  FFTPeak [ControlRate] buffer=0 freqlo=0 freqhi=50000
fftPeak :: Rate -> UGen -> UGen -> UGen -> UGen
fftPeak rate buffer freqlo freqhi = mkUGen Nothing [ControlRate] (Left rate) "FFTPeak" [buffer,freqlo,freqhi] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTPhaseDev [ControlRate] buffer=0 weight=0 powthresh=0.1
fftPhaseDev :: Rate -> UGen -> UGen -> UGen -> UGen
fftPhaseDev rate buffer weight powthresh = mkUGen Nothing [ControlRate] (Left rate) "FFTPhaseDev" [buffer,weight,powthresh] Nothing 1 (Special 0) NoId

-- | Instantaneous spectral power
--
--  FFTPower [ControlRate] buffer=0 square=1
fftPower :: Rate -> UGen -> UGen -> UGen
fftPower rate buffer square = mkUGen Nothing [ControlRate] (Left rate) "FFTPower" [buffer,square] Nothing 1 (Special 0) NoId

-- | Spectral slope
--
--  FFTSlope [ControlRate] buffer=0
fftSlope :: Rate -> UGen -> UGen
fftSlope rate buffer = mkUGen Nothing [ControlRate] (Left rate) "FFTSlope" [buffer] Nothing 1 (Special 0) NoId

-- | Spectral spread
--
--  FFTSpread [ControlRate] buffer=0 centroid=0
fftSpread :: Rate -> UGen -> UGen -> UGen
fftSpread rate buffer centroid = mkUGen Nothing [ControlRate] (Left rate) "FFTSpread" [buffer,centroid] Nothing 1 (Special 0) NoId

-- | Spectral flatness, divided into subbands
--
--  FFTSubbandFlatness [ControlRate] chain=0 cutfreqs=0
fftSubbandFlatness :: Rate -> UGen -> UGen -> UGen
fftSubbandFlatness rate chain cutfreqs = mkUGen Nothing [ControlRate] (Left rate) "FFTSubbandFlatness" [chain,cutfreqs] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTSubbandFlux [ControlRate] chain=0 cutfreqs=0 posonly=0
fftSubbandFlux :: Rate -> UGen -> UGen -> UGen -> UGen
fftSubbandFlux rate chain cutfreqs posonly = mkUGen Nothing [ControlRate] (Left rate) "FFTSubbandFlux" [chain,cutfreqs,posonly] Nothing 1 (Special 0) NoId

-- | Spectral power, divided into subbands
--
--  FFTSubbandPower [ControlRate] chain=0 cutfreqs=0 square=1 scalemode=1
fftSubbandPower :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
fftSubbandPower rate chain cutfreqs square scalemode = mkUGen Nothing [ControlRate] (Left rate) "FFTSubbandPower" [chain,cutfreqs,square,scalemode] Nothing 1 (Special 0) NoId

-- | Phase modulation oscillator matrix.
--
--  FM7 [AudioRate] *ctlMatrix=0 *modMatrix=0;    MCE=2
fm7 :: Rate -> UGen -> UGen -> UGen
fm7 rate ctlMatrix modMatrix = mkUGen Nothing [AudioRate] (Left rate) "FM7" [] (Just [ctlMatrix,modMatrix]) 6 (Special 0) NoId

-- | Granular synthesis with FM grains
--
--  FMGrain [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1;    FILTER: TRUE
fmGrain :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrain trigger dur carfreq modfreq index_ = mkUGen Nothing [AudioRate] (Right [0]) "FMGrain" [trigger,dur,carfreq,modfreq,index_] Nothing 1 (Special 0) NoId

-- | Granular synthesis with FM grains and user supplied envelope
--
--  FMGrainB [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 envbuf=0;    FILTER: TRUE
fmGrainB :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainB trigger dur carfreq modfreq index_ envbuf = mkUGen Nothing [AudioRate] (Right [0]) "FMGrainB" [trigger,dur,carfreq,modfreq,index_,envbuf] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FMGrainBBF [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 envbuf=0 azimuth=0 elevation=0 rho=1 wComp=0
fmGrainBBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainBBF rate trigger dur carfreq modfreq index_ envbuf azimuth elevation rho wComp = mkUGen Nothing [AudioRate] (Left rate) "FMGrainBBF" [trigger,dur,carfreq,modfreq,index_,envbuf,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  FMGrainBF [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 azimuth=0 elevation=0 rho=1 wComp=0
fmGrainBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainBF rate trigger dur carfreq modfreq index_ azimuth elevation rho wComp = mkUGen Nothing [AudioRate] (Left rate) "FMGrainBF" [trigger,dur,carfreq,modfreq,index_,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with FM grains and user supplied envelopes
--
--  FMGrainI [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 envbuf1=0 envbuf2=0 ifac=0.5
fmGrainI :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainI rate trigger dur carfreq modfreq index_ envbuf1 envbuf2 ifac = mkUGen Nothing [AudioRate] (Left rate) "FMGrainI" [trigger,dur,carfreq,modfreq,index_,envbuf1,envbuf2,ifac] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FMGrainIBF [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 envbuf1=0 envbuf2=0 ifac=0.5 azimuth=0 elevation=0 rho=1 wComp=0
fmGrainIBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmGrainIBF rate trigger dur carfreq modfreq index_ envbuf1 envbuf2 ifac azimuth elevation rho wComp = mkUGen Nothing [AudioRate] (Left rate) "FMGrainIBF" [trigger,dur,carfreq,modfreq,index_,envbuf1,envbuf2,ifac,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Decode an FMH signal for a specific speaker
--
--  FMHDecode1 [AudioRate] w=0 x=0 y=0 z=0 r=0 s=0 t=0 u=0 v=0 azimuth=0 elevation=0
fmhDecode1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmhDecode1 rate w x y z r s t u v azimuth elevation = mkUGen Nothing [AudioRate] (Left rate) "FMHDecode1" [w,x,y,z,r,s,t,u,v,azimuth,elevation] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FMHEncode0 [AudioRate] in=0 azimuth=0 elevation=0 gain=1
fmhEncode0 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
fmhEncode0 rate in_ azimuth elevation gain = mkUGen Nothing [AudioRate] (Left rate) "FMHEncode0" [in_,azimuth,elevation,gain] Nothing 9 (Special 0) NoId

-- | Second Order Ambisonic encoder
--
--  FMHEncode1 [AudioRate] in=0 azimuth=0 elevation=0 rho=1 gain=1 wComp=0
fmhEncode1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmhEncode1 rate in_ azimuth elevation rho gain wComp = mkUGen Nothing [AudioRate] (Left rate) "FMHEncode1" [in_,azimuth,elevation,rho,gain,wComp] Nothing 9 (Special 0) NoId

-- | Second Order Ambisonic encoder
--
--  FMHEncode2 [AudioRate] in=0 point_x=0 point_y=0 elevation=0 gain=1 wComp=0
fmhEncode2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fmhEncode2 rate in_ point_x point_y elevation gain wComp = mkUGen Nothing [AudioRate] (Left rate) "FMHEncode2" [in_,point_x,point_y,elevation,gain,wComp] Nothing 9 (Special 0) NoId

-- | Storing feature data from UGens in NRT mode
--
--  FeatureSave [ControlRate] features=0 trig=0
featureSave :: Rate -> UGen -> UGen -> UGen
featureSave rate features trig_ = mkUGen Nothing [ControlRate] (Left rate) "FeatureSave" [features,trig_] Nothing 1 (Special 0) NoId

-- | FitzHughNagumo Neuron Firing Oscillator
--
--  Fhn2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0 u0=0 w0=0
fhn2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fhn2DC rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Fhn2DC" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | FitzHughNagumo Neuron Firing Oscillator
--
--  Fhn2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0 u0=0 w0=0
fhn2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fhn2DL rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Fhn2DL" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | FitzHughNagumo Neuron Firing Oscillator
--
--  Fhn2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0 u0=0 w0=0
fhn2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fhn2DN rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Fhn2DN" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FhnTrig [ControlRate,AudioRate] minfreq=4 maxfreq=10 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0 u0=0 w0=0
fhnTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fhnTrig rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "FhnTrig" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FincoSprottL [AudioRate] freq=22050 a=2.45 h=0.05 xi=0 yi=0 zi=0
fincoSprottL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fincoSprottL rate freq a h xi yi zi = mkUGen Nothing [AudioRate] (Left rate) "FincoSprottL" [freq,a,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  FincoSprottM [AudioRate] freq=22050 a=-7 b=4 h=0.05 xi=0 yi=0 zi=0
fincoSprottM :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fincoSprottM rate freq a b h xi yi zi = mkUGen Nothing [AudioRate] (Left rate) "FincoSprottM" [freq,a,b,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  FincoSprottS [AudioRate] freq=22050 a=8 b=2 h=0.05 xi=0 yi=0 zi=0
fincoSprottS :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fincoSprottS rate freq a b h xi yi zi = mkUGen Nothing [AudioRate] (Left rate) "FincoSprottS" [freq,a,b,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | Neuron Firing Model Oscillator
--
--  FitzHughNagumo [AudioRate] reset=0 rateu=0.01 ratew=0.01 b0=1 b1=1 initu=0 initw=0
fitzHughNagumo :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fitzHughNagumo rate reset rateu ratew b0 b1 initu initw = mkUGen Nothing [AudioRate] (Left rate) "FitzHughNagumo" [reset,rateu,ratew,b0,b1,initu,initw] Nothing 1 (Special 0) NoId

-- | calculates spectral MSE distance of two fft chains
--
--  FrameCompare [ControlRate] buffer1=0 buffer2=0 wAmount=0.5
frameCompare :: Rate -> UGen -> UGen -> UGen -> UGen
frameCompare rate buffer1 buffer2 wAmount = mkUGen Nothing [ControlRate] (Left rate) "FrameCompare" [buffer1,buffer2,wAmount] Nothing 1 (Special 0) NoId

-- | A physical model of a system with dry-friction. A chaotic filter.
--
--  Friction [ControlRate,AudioRate] in=0 friction=0.5 spring=0.414 damp=0.313 mass=0.1 beltmass=1
friction :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
friction rate in_ friction_ spring_ damp mass beltmass = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Friction" [in_,friction_,spring_,damp,mass,beltmass] Nothing 1 (Special 0) NoId

-- | Single gammatone filter
--
--  Gammatone [AudioRate] input=0 centrefrequency=440 bandwidth=200;    FILTER: TRUE
gammatone :: UGen -> UGen -> UGen -> UGen
gammatone input centrefrequency bandwidth = mkUGen Nothing [AudioRate] (Right [0]) "Gammatone" [input,centrefrequency,bandwidth] Nothing 1 (Special 0) NoId

-- | Gaussian classifier
--
--  GaussClass [ControlRate] in=0 bufnum=0 gate=0
gaussClass :: Rate -> UGen -> UGen -> UGen -> UGen
gaussClass rate in_ bufnum gate_ = mkUGen Nothing [ControlRate] (Left rate) "GaussClass" [in_,bufnum,gate_] Nothing 1 (Special 0) NoId

-- | impulses around a certain frequency
--
--  GaussTrig [ControlRate,AudioRate] freq=440 dev=0.3
gaussTrig :: Rate -> UGen -> UGen -> UGen
gaussTrig rate freq dev = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "GaussTrig" [freq,dev] Nothing 1 (Special 0) NoId

-- | gingerbreadman map 2D chaotic generator
--
--  Gbman2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 x0=1.2 y0=2.1
gbman2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
gbman2DC rate minfreq maxfreq x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Gbman2DC" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | gingerbreadman map 2D chaotic generator
--
--  Gbman2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 x0=1.2 y0=2.1
gbman2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
gbman2DL rate minfreq maxfreq x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Gbman2DL" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | gingerbreadman map 2D chaotic generator
--
--  Gbman2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 x0=1.2 y0=2.1
gbman2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
gbman2DN rate minfreq maxfreq x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Gbman2DN" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GbmanTrig [ControlRate,AudioRate] minfreq=5 maxfreq=10 x0=1.2 y0=2.1
gbmanTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
gbmanTrig rate minfreq maxfreq x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "GbmanTrig" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | Dynamic stochastic synthesis generator
--
--  Gendy4 [ControlRate,AudioRate] ampdist=1 durdist=1 adparam=1 ddparam=1 minfreq=440 maxfreq=660 ampscale=0.5 durscale=0.5 initCPs=12 knum=0
gendy4 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy4 rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Gendy4" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) NoId

-- | Dynamic stochastic synthesis generator
--
--  Gendy5 [ControlRate,AudioRate] ampdist=1 durdist=1 adparam=1 ddparam=1 minfreq=440 maxfreq=660 ampscale=0.5 durscale=0.5 initCPs=12 knum=0
gendy5 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy5 rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Gendy5" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) NoId

-- | Read (numeric) shell environment variables into a synth
--
--  Getenv [] key=0 defaultval=0
getenv :: Rate -> UGen -> UGen -> UGen
getenv rate key defaultval = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "Getenv" [key,defaultval] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchBPF [ControlRate,AudioRate] in=0 freq=440 rq=1
glitchBPF :: Rate -> UGen -> UGen -> UGen -> UGen
glitchBPF rate in_ freq rq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "GlitchBPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchBRF [ControlRate,AudioRate] in=0 freq=440 rq=1
glitchBRF :: Rate -> UGen -> UGen -> UGen -> UGen
glitchBRF rate in_ freq rq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "GlitchBRF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchHPF [ControlRate,AudioRate] in=0 freq=440
glitchHPF :: Rate -> UGen -> UGen -> UGen
glitchHPF rate in_ freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "GlitchHPF" [in_,freq] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchRHPF [ControlRate,AudioRate] in=0 freq=440 rq=1;    FILTER: TRUE
glitchRHPF :: UGen -> UGen -> UGen -> UGen
glitchRHPF in_ freq rq = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "GlitchRHPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Calculate a single DFT bin, to detect presence of a frequency
--
--  Goertzel [ControlRate] in=0 bufsize=1024 freq=0 hop=1
goertzel :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
goertzel rate in_ bufsize freq hop = mkUGen Nothing [ControlRate] (Left rate) "Goertzel" [in_,bufsize,freq,hop] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainBufJ [AudioRate] numChannels=1 trigger=0 dur=1 sndbuf=0 rate=1 pos=0 loop=0 interp=2 grainAmp=1 pan=0 envbufnum=-1 maxGrains=512
grainBufJ :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainBufJ rate numChannels trigger dur sndbuf rate_ pos loop interp grainAmp pan envbufnum maxGrains = mkUGen Nothing [AudioRate] (Left rate) "GrainBufJ" [numChannels,trigger,dur,sndbuf,rate_,pos,loop,interp,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainFMJ [AudioRate] numChannels=1 trigger=0 dur=1 carfreq=440 modfreq=200 index=1 grainAmp=1 pan=0 envbufnum=-1 maxGrains=512
grainFMJ :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainFMJ rate numChannels trigger dur carfreq modfreq index_ grainAmp pan envbufnum maxGrains = mkUGen Nothing [AudioRate] (Left rate) "GrainFMJ" [numChannels,trigger,dur,carfreq,modfreq,index_,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainInJ [AudioRate] numChannels=1 trigger=0 dur=1 in=0 grainAmp=1 pan=0 envbufnum=-1 maxGrains=512
grainInJ :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainInJ rate numChannels trigger dur in_ grainAmp pan envbufnum maxGrains = mkUGen Nothing [AudioRate] (Left rate) "GrainInJ" [numChannels,trigger,dur,in_,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainSinJ [AudioRate] numChannels=1 trigger=0 dur=1 freq=440 grainAmp=1 pan=0 envbufnum=-1 maxGrains=512
grainSinJ :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainSinJ rate numChannels trigger dur freq grainAmp pan envbufnum maxGrains = mkUGen Nothing [AudioRate] (Left rate) "GrainSinJ" [numChannels,trigger,dur,freq,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | dynamical system simulation (Newtonian gravitational force)
--
--  GravityGrid [AudioRate] reset=0 rate=0.1 newx=0 newy=0 bufnum=0
gravityGrid :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gravityGrid rate reset rate_ newx newy bufnum = mkUGen Nothing [AudioRate] (Left rate) "GravityGrid" [reset,rate_,newx,newy,bufnum] Nothing 1 (Special 0) NoId

-- | dynamical system simulation (Newtonian gravitational force)
--
--  GravityGrid2 [AudioRate] reset=0 rate=0.1 newx=0 newy=0 bufnum=0
gravityGrid2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gravityGrid2 rate reset rate_ newx newy bufnum = mkUGen Nothing [AudioRate] (Left rate) "GravityGrid2" [reset,rate_,newx,newy,bufnum] Nothing 1 (Special 0) NoId

-- | algorithmic delay
--
--  GreyholeRaw [AudioRate] in1=0 in2=0 damping=0 delaytime=2 diffusion=0.5 feedback=0.9 moddepth=0.1 modfreq=2 size=1;    FILTER: TRUE
greyholeRaw :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
greyholeRaw in1 in2 damping delaytime diffusion feedback moddepth modfreq size = mkUGen Nothing [AudioRate] (Right [0,1]) "GreyholeRaw" [in1,in2,damping,delaytime,diffusion,feedback,moddepth,modfreq,size] Nothing 2 (Special 0) NoId

-- | Simple cochlear hair cell model
--
--  HairCell [ControlRate,AudioRate] input=0 spontaneousrate=0 boostrate=200 restorerate=1000 loss=0.99;    FILTER: TRUE
hairCell :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
hairCell input spontaneousrate boostrate restorerate loss = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "HairCell" [input,spontaneousrate,boostrate,restorerate,loss] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  HarmonicOsc [ControlRate,AudioRate] freq=100 firstharmonic=1 *amplitudes=0;    MCE=1
harmonicOsc :: Rate -> UGen -> UGen -> UGen -> UGen
harmonicOsc rate freq firstharmonic amplitudes = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "HarmonicOsc" [freq,firstharmonic] (Just [amplitudes]) 1 (Special 0) NoId

-- | henon map 2D chaotic generator
--
--  Henon2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1.4 b=0.3 x0=0.30502 y0=0.20939
henon2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henon2DC rate minfreq maxfreq a b x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Henon2DC" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | henon map 2D chaotic generator
--
--  Henon2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1.4 b=0.3 x0=0.30502 y0=0.20939
henon2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henon2DL rate minfreq maxfreq a b x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Henon2DL" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | henon map 2D chaotic generator
--
--  Henon2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1.4 b=0.3 x0=0.30502 y0=0.20939
henon2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henon2DN rate minfreq maxfreq a b x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Henon2DN" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  HenonTrig [ControlRate,AudioRate] minfreq=5 maxfreq=10 a=1.4 b=0.3 x0=0.30502 y0=0.20939
henonTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonTrig rate minfreq maxfreq a b x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "HenonTrig" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | Transform a cepstrum back to a spectrum
--
--  ICepstrum [] cepchain=0 fftbuf=0
iCepstrum :: Rate -> UGen -> UGen -> UGen
iCepstrum rate cepchain fftbuf = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "ICepstrum" [cepchain,fftbuf] Nothing 1 (Special 0) NoId

-- | 24db/oct rolloff, 4nd order resonant Low Pass Filter
--
--  IirFilter [AudioRate] in=0 freq=440 rq=1;    FILTER: TRUE
iirFilter :: UGen -> UGen -> UGen -> UGen
iirFilter in_ freq rq = mkUGen Nothing [AudioRate] (Right [0]) "IirFilter" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrain [AudioRate] trigger=0 dur=1 in=0
inGrain :: Rate -> UGen -> UGen -> UGen -> UGen
inGrain rate trigger dur in_ = mkUGen Nothing [AudioRate] (Left rate) "InGrain" [trigger,dur,in_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainB [AudioRate] trigger=0 dur=1 in=0 envbuf=0
inGrainB :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
inGrainB rate trigger dur in_ envbuf = mkUGen Nothing [AudioRate] (Left rate) "InGrainB" [trigger,dur,in_,envbuf] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainBBF [AudioRate] trigger=0 dur=1 in=0 envbuf=0 azimuth=0 elevation=0 rho=1 wComp=0
inGrainBBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
inGrainBBF rate trigger dur in_ envbuf azimuth elevation rho wComp = mkUGen Nothing [AudioRate] (Left rate) "InGrainBBF" [trigger,dur,in_,envbuf,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainBF [AudioRate] trigger=0 dur=1 in=0 azimuth=0 elevation=0 rho=1 wComp=0
inGrainBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
inGrainBF rate trigger dur in_ azimuth elevation rho wComp = mkUGen Nothing [AudioRate] (Left rate) "InGrainBF" [trigger,dur,in_,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainI [AudioRate] trigger=0 dur=1 in=0 envbuf1=0 envbuf2=0 ifac=0.5
inGrainI :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
inGrainI rate trigger dur in_ envbuf1 envbuf2 ifac = mkUGen Nothing [AudioRate] (Left rate) "InGrainI" [trigger,dur,in_,envbuf1,envbuf2,ifac] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainIBF [AudioRate] trigger=0 dur=1 in=0 envbuf1=0 envbuf2=0 ifac=0.5 azimuth=0 elevation=0 rho=1 wComp=0
inGrainIBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
inGrainIBF rate trigger dur in_ envbuf1 envbuf2 ifac azimuth elevation rho wComp = mkUGen Nothing [AudioRate] (Left rate) "InGrainIBF" [trigger,dur,in_,envbuf1,envbuf2,ifac,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Distortion by subtracting magnitude from 1
--
--  InsideOut [ControlRate,AudioRate] in=0
insideOut :: Rate -> UGen -> UGen
insideOut rate in_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "InsideOut" [in_] Nothing 1 (Special 0) NoId

-- | instruction synthesis (breakpoint set interpreter)
--
--  Instruction [AudioRate] bufnum=0
instruction :: Rate -> UGen -> UGen
instruction rate bufnum = mkUGen Nothing [AudioRate] (Left rate) "Instruction" [bufnum] Nothing 1 (Special 0) NoId

-- | Raw version of the JPverb algorithmic reverberator, designed to produce long tails with chorusing
--
--  JPverbRaw [ControlRate,AudioRate] in1=0 in2=0 damp=0 earlydiff=0.707 highband=2000 highx=1 lowband=500 lowx=1 mdepth=0.1 mfreq=2 midx=1 size=1 t60=1;    FILTER: TRUE
jPverbRaw :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
jPverbRaw in1 in2 damp earlydiff highband highx lowband lowx mdepth mfreq midx size t60 = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "JPverbRaw" [in1,in2,damp,earlydiff,highband,highx,lowband,lowx,mdepth,mfreq,midx,size,t60] Nothing 2 (Special 0) NoId

-- | k-means classification in real time
--
--  KMeansRT [ControlRate] bufnum=0 inputdata=0 k=5 gate=1 reset=0 learn=1
kMeansRT :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
kMeansRT rate bufnum inputdata k gate_ reset learn = mkUGen Nothing [ControlRate] (Left rate) "KMeansRT" [bufnum,inputdata,k,gate_,reset,learn] Nothing 1 (Special 0) NoId

-- | Running score of maximum correlation of chromagram with key profiles
--
--  KeyClarity [ControlRate] chain=0 keydecay=2 chromaleak=0.5
keyClarity :: Rate -> UGen -> UGen -> UGen -> UGen
keyClarity rate chain keydecay chromaleak = mkUGen Nothing [ControlRate] (Left rate) "KeyClarity" [chain,keydecay,chromaleak] Nothing 1 (Special 0) NoId

-- | Find best correlated key mode with chromagram between major, minor and chromatic cluster
--
--  KeyMode [ControlRate] chain=0 keydecay=2 chromaleak=0.5
keyMode :: Rate -> UGen -> UGen -> UGen -> UGen
keyMode rate chain keydecay chromaleak = mkUGen Nothing [ControlRate] (Left rate) "KeyMode" [chain,keydecay,chromaleak] Nothing 1 (Special 0) NoId

-- | K-means Oscillator
--
--  KmeansToBPSet1 [AudioRate] freq=440 numdatapoints=20 maxnummeans=4 nummeans=4 tnewdata=1 tnewmeans=1 soft=1 bufnum=0
kmeansToBPSet1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
kmeansToBPSet1 rate freq numdatapoints maxnummeans nummeans tnewdata tnewmeans soft bufnum = mkUGen Nothing [AudioRate] (Left rate) "KmeansToBPSet1" [freq,numdatapoints,maxnummeans,nummeans,tnewdata,tnewmeans,soft,bufnum] Nothing 1 (Special 0) NoId

{-
-- | Run any LADSPA plugin inside SuperCollider
--
--  LADSPA [AudioRate] nChans=0 id=0 args=0
ladspa :: Rate -> UGen -> UGen -> UGen -> UGen
ladspa rate nChans id_ args = mkUGen Nothing [AudioRate] (Left rate) "LADSPA" [nChans,id_,args] Nothing 0 (Special 0) NoId
-}

-- | random walk step
--
--  LFBrownNoise0 [ControlRate,AudioRate] freq=20 dev=1 dist=0;    NONDET
lfBrownNoise0Id :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise0Id z rate freq dev dist = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFBrownNoise0" [freq,dev,dist] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFBrownNoise0.
lfBrownNoise0M :: UId m => Rate -> UGen -> UGen -> UGen -> m UGen
lfBrownNoise0M = liftUId4 lfBrownNoise0Id

-- | Unsafe variant of LFBrownNoise0.
lfBrownNoise0 ::  Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise0 = liftUnsafe4 lfBrownNoise0M

-- | random walk linear interp
--
--  LFBrownNoise1 [ControlRate,AudioRate] freq=20 dev=1 dist=0;    NONDET
lfBrownNoise1Id :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise1Id z rate freq dev dist = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFBrownNoise1" [freq,dev,dist] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFBrownNoise1.
lfBrownNoise1M :: UId m => Rate -> UGen -> UGen -> UGen -> m UGen
lfBrownNoise1M = liftUId4 lfBrownNoise1Id

-- | Unsafe variant of LFBrownNoise1.
lfBrownNoise1 ::  Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise1 = liftUnsafe4 lfBrownNoise1M

-- | random walk cubic interp
--
--  LFBrownNoise2 [ControlRate,AudioRate] freq=20 dev=1 dist=0;    NONDET
lfBrownNoise2Id :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise2Id z rate freq dev dist = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LFBrownNoise2" [freq,dev,dist] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of LFBrownNoise2.
lfBrownNoise2M :: UId m => Rate -> UGen -> UGen -> UGen -> m UGen
lfBrownNoise2M = liftUId4 lfBrownNoise2Id

-- | Unsafe variant of LFBrownNoise2.
lfBrownNoise2 ::  Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise2 = liftUnsafe4 lfBrownNoise2M

-- | Live Linear Predictive Coding Analysis and Resynthesis
--
--  LPCAnalyzer [AudioRate] input=0 source=0.01 n=256 p=10 testE=0 delta=0.999 windowtype=0;    FILTER: TRUE
lpcAnalyzer :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lpcAnalyzer input source n p testE delta windowtype = mkUGen Nothing [AudioRate] (Right [0,1]) "LPCAnalyzer" [input,source,n,p,testE,delta,windowtype] Nothing 1 (Special 0) NoId

-- | Linear Predictive Coding Gone Wrong
--
--  LPCError [AudioRate] input=0 p=10
lpcError :: Rate -> UGen -> UGen -> UGen
lpcError rate input p = mkUGen Nothing [AudioRate] (Left rate) "LPCError" [input,p] Nothing 1 (Special 0) NoId

-- | Utilize LPC data
--
--  LPCSynth [AudioRate] buffer=0 signal=0 pointer=0
lpcSynth :: UGen -> UGen -> UGen -> UGen
lpcSynth buffer signal pointer = mkUGen Nothing [AudioRate] (Left AudioRate) "LPCSynth" [buffer,signal,pointer] Nothing 1 (Special 0) NoId

-- | Utilize LPC data
--
--  LPCVals [ControlRate,AudioRate] buffer=0 pointer=0
lpcVals :: UGen -> UGen -> UGen
lpcVals buffer pointer = mkUGen Nothing [ControlRate,AudioRate] (Left AudioRate) "LPCVals" [buffer,pointer] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  LPF1 [ControlRate,AudioRate] in=0 freq=1000
lpf1 :: Rate -> UGen -> UGen -> UGen
lpf1 rate in_ freq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LPF1" [in_,freq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LPF18 [AudioRate] in=0 freq=100 res=1 dist=0.4
lpf18 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
lpf18 rate in_ freq res dist = mkUGen Nothing [AudioRate] (Left rate) "LPF18" [in_,freq,res,dist] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LPFVS6 [ControlRate,AudioRate] in=0 freq=1000 slope=0.5
lpfvs6 :: Rate -> UGen -> UGen -> UGen -> UGen
lpfvs6 rate in_ freq slope_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LPFVS6" [in_,freq,slope_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LPG [AudioRate] input=0 controlinput=0 controloffset=0 controlscale=1 vca=1 resonance=1.5 lowpassmode=1 linearity=1;    FILTER: TRUE
lpg :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lpg input controlinput controloffset controlscale vca resonance lowpassmode linearity = mkUGen Nothing [AudioRate] (Right [0]) "LPG" [input,controlinput,controloffset,controlscale,vca,resonance,lowpassmode,linearity] Nothing 1 (Special 0) NoId

-- | Linear Time Invariant General Filter Equation
--
--  LTI [AudioRate] input=0 bufnuma=0 bufnumb=1
lti :: Rate -> UGen -> UGen -> UGen -> UGen
lti rate input bufnuma bufnumb = mkUGen Nothing [AudioRate] (Left rate) "LTI" [input,bufnuma,bufnumb] Nothing 1 (Special 0) NoId

-- | latoocarfian 2D chaotic generator
--
--  Latoocarfian2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1 b=3 c=0.5 d=0.5 x0=0.34082 y0=-0.3827
latoocarfian2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfian2DC rate minfreq maxfreq a b c d x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Latoocarfian2DC" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | latoocarfian 2D chaotic generator
--
--  Latoocarfian2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1 b=3 c=0.5 d=0.5 x0=0.34082 y0=-0.3827
latoocarfian2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfian2DL rate minfreq maxfreq a b c d x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Latoocarfian2DL" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | latoocarfian 2D chaotic generator
--
--  Latoocarfian2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1 b=3 c=0.5 d=0.5 x0=0.34082 y0=-0.3827
latoocarfian2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfian2DN rate minfreq maxfreq a b c d x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Latoocarfian2DN" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LatoocarfianTrig [ControlRate,AudioRate] minfreq=5 maxfreq=10 a=1 b=3 c=0.5 d=0.5 x0=0.34082 y0=-0.3827
latoocarfianTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianTrig rate minfreq maxfreq a b c d x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LatoocarfianTrig" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | Emit a sequence of triggers at specified time offsets
--
--  ListTrig [ControlRate] bufnum=0 reset=0 offset=0 numframes=0
listTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
listTrig rate bufnum reset offset numframes = mkUGen Nothing [ControlRate] (Left rate) "ListTrig" [bufnum,reset,offset,numframes] Nothing 1 (Special 0) NoId

-- | Emit a sequence of triggers at specified time offsets
--
--  ListTrig2 [ControlRate] bufnum=0 reset=0 numframes=0
listTrig2 :: Rate -> UGen -> UGen -> UGen -> UGen
listTrig2 rate bufnum reset numframes = mkUGen Nothing [ControlRate] (Left rate) "ListTrig2" [bufnum,reset,numframes] Nothing 1 (Special 0) NoId

-- | Store values to a buffer, whenever triggered
--
--  Logger [ControlRate] inputArray=0 trig=0 bufnum=0 reset=0
logger :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
logger rate inputArray trig_ bufnum reset = mkUGen Nothing [ControlRate] (Left rate) "Logger" [inputArray,trig_,bufnum,reset] Nothing 1 (Special 0) NoId

-- | sample looping oscillator
--
--  LoopBuf [AudioRate] bufnum=0 rate=1 gate=1 startPos=0 startLoop=0 endLoop=0 interpolation=2;    NC INPUT: True
loopBuf :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
loopBuf numChannels rate bufnum rate_ gate_ startPos startLoop endLoop interpolation = mkUGen Nothing [AudioRate] (Left rate) "LoopBuf" [bufnum,rate_,gate_,startPos,startLoop,endLoop,interpolation] Nothing numChannels (Special 0) NoId

-- | lorenz 2D chaotic generator
--
--  Lorenz2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 s=10 r=28 b=2.66667 h=0.02 x0=0.09088 y0=2.97077 z0=24.28204
lorenz2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenz2DC rate minfreq maxfreq s r b h x0 y0 z0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Lorenz2DC" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | lorenz 2D chaotic generator
--
--  Lorenz2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 s=10 r=28 b=2.66667 h=0.02 x0=0.09088 y0=2.97077 z0=24.28204
lorenz2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenz2DL rate minfreq maxfreq s r b h x0 y0 z0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Lorenz2DL" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | lorenz 2D chaotic generator
--
--  Lorenz2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 s=10 r=28 b=2.66667 h=0.02 x0=0.09088 y0=2.97077 z0=24.28204
lorenz2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenz2DN rate minfreq maxfreq s r b h x0 y0 z0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Lorenz2DN" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | lorenz chaotic trigger generator
--
--  LorenzTrig [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 s=10 r=28 b=2.66667 h=0.02 x0=0.09088 y0=2.97077 z0=24.28204
lorenzTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenzTrig rate minfreq maxfreq s r b h x0 y0 z0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "LorenzTrig" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | simple resonating lowpass filter
--
--  Lores [AudioRate] in=0 freq=880 res=0.5;    FILTER: TRUE
lores :: UGen -> UGen -> UGen -> UGen
lores in_ freq res = mkUGen Nothing [AudioRate] (Right [0]) "Lores" [in_,freq,res] Nothing 1 (Special 0) NoId

-- | 2-species Predator-Prey model
--
--  LotkaVolterra [AudioRate] freq=22050 a=1.5 b=1.5 c=0.5 d=1.5 h=0.05 xi=1 yi=0.2
lotkaVolterra :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lotkaVolterra rate freq a b c d h xi yi = mkUGen Nothing [AudioRate] (Left rate) "LotkaVolterra" [freq,a,b,c,d,h,xi,yi] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  MCLDChaosGen [] maxSize=0
mcldChaosGen :: Rate -> UGen -> UGen
mcldChaosGen rate maxSize = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "MCLDChaosGen" [maxSize] Nothing 1 (Special 0) NoId

-- | POKEY Chip Sound Simulator
--
--  MZPokey [AudioRate] audf1=0 audc1=0 audf2=0 audc2=0 audf3=0 audc3=0 audf4=0 audc4=0 audctl=0
mzPokey :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
mzPokey rate audf1 audc1 audf2 audc2 audf3 audc3 audf4 audc4 audctl = mkUGen Nothing [AudioRate] (Left rate) "MZPokey" [audf1,audc1,audf2,audc2,audf3,audc3,audf4,audc4,audctl] Nothing 1 (Special 0) NoId

-- | First order Markov Chain implementation for audio signals
--
--  MarkovSynth [AudioRate] in=0 isRecording=1 waitTime=2 tableSize=10
markovSynth :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
markovSynth rate in_ isRecording waitTime tableSize = mkUGen Nothing [AudioRate] (Left rate) "MarkovSynth" [in_,isRecording,waitTime,tableSize] Nothing 1 (Special 0) NoId

-- | Real time sparse representation
--
--  MatchingP [ControlRate,AudioRate] dict=0 in=0 dictsize=1 ntofind=1 hop=1 method=0
matchingP :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
matchingP rate dict in_ dictsize ntofind hop method = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "MatchingP" [dict,in_,dictsize,ntofind,hop,method] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  MatchingPResynth [ControlRate,AudioRate] dict=0 method=0 trigger=0 residual=0 activs=0
matchingPResynth :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
matchingPResynth rate dict method trigger residual activs = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "MatchingPResynth" [dict,method,trigger,residual,activs] Nothing 1 (Special 0) NoId

-- | maximum within last x samples
--
--  Max [ControlRate] in=0 numsamp=64
max :: Rate -> UGen -> UGen -> UGen
max rate in_ numsamp = mkUGen Nothing [ControlRate] (Left rate) "Max" [in_,numsamp] Nothing 1 (Special 0) NoId

-- | Tracks and prints amplitudes
--
--  Maxamp [AudioRate] in=0 numSamps=1000
maxamp :: Rate -> UGen -> UGen -> UGen
maxamp rate in_ numSamps = mkUGen Nothing [AudioRate] (Left rate) "Maxamp" [in_,numSamps] Nothing 1 (Special 0) NoId

-- | Piano synthesiser
--
--  MdaPiano [AudioRate] freq=440 gate=1 vel=100 decay=0.8 release=0.8 hard=0.8 velhard=0.8 muffle=0.8 velmuff=0.8 velcurve=0.8 stereo=0.2 tune=0.5 random=0.1 stretch=0.1 sustain=0
mdaPiano :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
mdaPiano rate freq gate_ vel decay_ release hard velhard muffle velmuff velcurve stereo tune random stretch sustain = mkUGen Nothing [AudioRate] (Left rate) "MdaPiano" [freq,gate_,vel,decay_,release,hard,velhard,muffle,velmuff,velcurve,stereo,tune,random,stretch,sustain] Nothing 2 (Special 0) NoId

-- | Mean of recent values, triggered
--
--  MeanTriggered [ControlRate,AudioRate] in=0 trig=0 length=10
meanTriggered :: Rate -> UGen -> UGen -> UGen -> UGen
meanTriggered rate in_ trig_ length_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "MeanTriggered" [in_,trig_,length_] Nothing 1 (Special 0) NoId

-- | Meddis cochlear hair cell model
--
--  Meddis [ControlRate,AudioRate] input=0;    FILTER: TRUE
meddis :: UGen -> UGen
meddis input = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Meddis" [input] Nothing 1 (Special 0) NoId

-- | Separate harmonic and percussive parts of a signal
--
--  MedianSeparation [] fft=0 fftharmonic=0 fftpercussive=0 fftsize=1024 mediansize=17 hardorsoft=0 p=2 medianormax=0
medianSeparation :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
medianSeparation rate fft_ fftharmonic fftpercussive fftsize mediansize hardorsoft p medianormax = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "MedianSeparation" [fft_,fftharmonic,fftpercussive,fftsize,mediansize,hardorsoft,p,medianormax] Nothing 2 (Special 0) NoId

-- | Median of recent values, triggered
--
--  MedianTriggered [ControlRate,AudioRate] in=0 trig=0 length=10
medianTriggered :: Rate -> UGen -> UGen -> UGen -> UGen
medianTriggered rate in_ trig_ length_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "MedianTriggered" [in_,trig_,length_] Nothing 1 (Special 0) NoId

-- | Waveguide mesh physical models of drum membranes
--
--  MembraneCircle [AudioRate] excitation=0 tension=0.05 loss=0.99999
membraneCircle :: Rate -> UGen -> UGen -> UGen -> UGen
membraneCircle rate excitation tension loss = mkUGen Nothing [AudioRate] (Left rate) "MembraneCircle" [excitation,tension,loss] Nothing 1 (Special 0) NoId

-- | Waveguide mesh physical models of drum membranes
--
--  MembraneHexagon [AudioRate] excitation=0 tension=0.05 loss=0.99999
membraneHexagon :: Rate -> UGen -> UGen -> UGen -> UGen
membraneHexagon rate excitation tension loss = mkUGen Nothing [AudioRate] (Left rate) "MembraneHexagon" [excitation,tension,loss] Nothing 1 (Special 0) NoId

-- | Metronome
--
--  Metro [ControlRate,AudioRate] bpm=0 numBeats=0
metro :: Rate -> UGen -> UGen -> UGen
metro rate bpm numBeats = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Metro" [bpm,numBeats] Nothing 1 (Special 0) NoId

-- | a macro oscillator
--
--  MiBraids [AudioRate] pitch=60 timbre=0.5 color=0.5 model=0 trig=0 resamp=0 decim=0 bits=0 ws=0
miBraids :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miBraids rate pitch_ timbre color model trig_ resamp decim bits ws = mkUGen Nothing [AudioRate] (Left rate) "MiBraids" [pitch_,timbre,color,model,trig_,resamp,decim,bits,ws] Nothing 1 (Special 0) NoId

-- | granular audio processor and texture synthesizer
--
--  MiClouds [AudioRate] pit=0 pos=0.5 size=0.25 dens=0.4 tex=0.5 drywet=0.5 in_gain=1 spread=0.5 rvb=0 fb=0 freeze=0 mode=0 lofi=0 trig=0 *inputArray=0;    MCE=1, REORDERS INPUTS: [14,0,1,2,3,4,5,6,7,8,9,10,11,12,13]
miClouds :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miClouds rate pit pos size dens tex drywet in_gain spread rvb fb freeze mode lofi trig_ inputArray = mkUGen Nothing [AudioRate] (Left rate) "MiClouds" [pit,pos,size,dens,tex,drywet,in_gain,spread,rvb,fb,freeze,mode,lofi,trig_] (Just [inputArray]) 2 (Special 0) NoId

-- | Physical modelling based on Modal Synthesis.
--
--  MiElements [AudioRate] blow_in=0 strike_in=0 gate=0 pit=48 strength=0.5 contour=0.2 bow_level=0 blow_level=0 strike_level=0 flow=0.5 mallet=0.5 bow_timb=0.5 blow_timb=0.5 strike_timb=0.5 geom=0.25 bright=0.5 damp=0.7 pos=0.2 space=0.3 model=0 easteregg=0
miElements :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miElements rate blow_in strike_in gate_ pit strength contour bow_level blow_level strike_level flow mallet bow_timb blow_timb strike_timb geom bright damp pos space model easteregg = mkUGen Nothing [AudioRate] (Left rate) "MiElements" [blow_in,strike_in,gate_,pit,strength,contour,bow_level,blow_level,strike_level,flow,mallet,bow_timb,blow_timb,strike_timb,geom,bright,damp,pos,space,model,easteregg] Nothing 2 (Special 0) NoId

-- | topographic drum sequencer
--
--  MiGrids [AudioRate] bpm=120 map_x=0.5 map_y=0.5 chaos=0 bd_density=0.25 sd_density=0.25 hh_density=0.25 mode=1 swing=0 config=0
miGrids :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miGrids rate bpm map_x map_y chaos bd_density sd_density hh_density mode swing config = mkUGen Nothing [AudioRate] (Left rate) "MiGrids" [bpm,map_x,map_y,chaos,bd_density,sd_density,hh_density,mode,swing,config] Nothing 6 (Special 0) NoId

-- | µ-law audio companding
--
--  MiMu [AudioRate] in=0 gain=1 bypass=0
miMu :: Rate -> UGen -> UGen -> UGen -> UGen
miMu rate in_ gain bypass = mkUGen Nothing [AudioRate] (Left rate) "MiMu" [in_,gain,bypass] Nothing 1 (Special 0) NoId

-- | FM Synth-Voice based on 'ominous'
--
--  MiOmi [AudioRate] audio_in=0 gate=0 pit=48 contour=0.2 detune=0.25 level1=0.5 level2=0.5 ratio1=0.5 ratio2=0.5 fm1=0 fm2=0 fb=0 xfb=0 filter_mode=0 cutoff=0.5 reson=0 strength=0.5 env=0.5 rotate=0.2 space=0.5
miOmi :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miOmi rate audio_in gate_ pit contour detune level1 level2 ratio1 ratio2 fm1 fm2 fb xfb filter_mode cutoff reson strength env rotate_ space = mkUGen Nothing [AudioRate] (Left rate) "MiOmi" [audio_in,gate_,pit,contour,detune,level1,level2,ratio1,ratio2,fm1,fm2,fb,xfb,filter_mode,cutoff,reson,strength,env,rotate_,space] Nothing 2 (Special 0) NoId

-- | a macro oscillator
--
--  MiPlaits [AudioRate] pitch=60 engine=0 harm=0.1 timbre=0.5 morph=0.5 trigger=0 level=0 fm_mod=0 timb_mod=0 morph_mod=0 decay=0.5 lpg_colour=0.5
miPlaits :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miPlaits rate pitch_ engine harm timbre morph trigger level fm_mod timb_mod morph_mod decay_ lpg_colour = mkUGen Nothing [AudioRate] (Left rate) "MiPlaits" [pitch_,engine,harm,timbre,morph,trigger,level,fm_mod,timb_mod,morph_mod,decay_,lpg_colour] Nothing 2 (Special 0) NoId

-- | a resonator
--
--  MiRings [AudioRate] in=0 trig=0 pit=60 struct=0.25 bright=0.5 damp=0.7 pos=0.25 model=0 poly=1 intern_exciter=0 easteregg=0 bypass=0
miRings :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miRings rate in_ trig_ pit struct bright damp pos model poly intern_exciter easteregg bypass = mkUGen Nothing [AudioRate] (Left rate) "MiRings" [in_,trig_,pit,struct,bright,damp,pos,model,poly,intern_exciter,easteregg,bypass] Nothing 2 (Special 0) NoId

-- | Classic resonant LP filter
--
--  MiRipples [AudioRate] in=0 cf=0.3 reson=0.2 drive=1;    FILTER: TRUE
miRipples :: UGen -> UGen -> UGen -> UGen -> UGen
miRipples in_ cf reson drive = mkUGen Nothing [AudioRate] (Right [0]) "MiRipples" [in_,cf,reson,drive] Nothing 1 (Special 0) NoId

-- | a quad LFO
--
--  MiTides [AudioRate] freq=1 shape=0.5 slope=0.5 smooth=0.5 shift=0.2 trig=0 clock=0 output_mode=3 ramp_mode=1 ratio=9 rate=1
miTides :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miTides rate freq shape slope_ smooth shift trig_ clock output_mode ramp_mode ratio rate_ = mkUGen Nothing [AudioRate] (Left rate) "MiTides" [freq,shape,slope_,smooth,shift,trig_,clock,output_mode,ramp_mode,ratio,rate_] Nothing 4 (Special 0) NoId

-- | stereo reverb
--
--  MiVerb [AudioRate] time=0.7 drywet=0.5 damp=0.5 hp=0.05 freeze=0 diff=0.625 *inputArray=0;    MCE=1, FILTER: TRUE, REORDERS INPUTS: [6,0,1,2,3,4,5]
miVerb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miVerb time drywet damp hp freeze diff inputArray = mkUGen Nothing [AudioRate] (Right [6]) "MiVerb" [time,drywet,damp,hp,freeze,diff] (Just [inputArray]) 2 (Special 0) NoId

-- | a meta modulator
--
--  MiWarps [AudioRate] carrier=0 modulator=0 lev1=0.5 lev2=0.5 algo=0 timb=0 osc=0 freq=110 vgain=1 easteregg=0
miWarps :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
miWarps rate carrier modulator lev1 lev2 algo timb osc_ freq vgain easteregg = mkUGen Nothing [AudioRate] (Left rate) "MiWarps" [carrier,modulator,lev1,lev2,algo,timb,osc_,freq,vgain,easteregg] Nothing 2 (Special 0) NoId

-- | Granulates real-time input
--
--  MonoGrain [AudioRate] in=0 winsize=0.1 grainrate=10 winrandpct=0
monoGrain :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
monoGrain rate in_ winsize grainrate winrandpct = mkUGen Nothing [AudioRate] (Left rate) "MonoGrain" [in_,winsize,grainrate,winrandpct] Nothing 1 (Special 0) NoId

-- | Granulates real-time input with Ambisonic panning
--
--  MonoGrainBF [AudioRate] in=0 winsize=0.1 grainrate=10 winrandpct=0 azimuth=0 azrand=0 elevation=0 elrand=0 rho=1
monoGrainBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
monoGrainBF rate in_ winsize grainrate winrandpct azimuth azrand elevation elrand rho = mkUGen Nothing [AudioRate] (Left rate) "MonoGrainBF" [in_,winsize,grainrate,winrandpct,azimuth,azrand,elevation,elrand,rho] Nothing 4 (Special 0) NoId

-- | Moog Filter Emulation
--
--  MoogLadder [ControlRate,AudioRate] in=0 ffreq=440 res=0;    FILTER: TRUE
moogLadder :: UGen -> UGen -> UGen -> UGen
moogLadder in_ ffreq res = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "MoogLadder" [in_,ffreq,res] Nothing 1 (Special 0) NoId

-- | Moog  filter emulation
--
--  MoogVCF [AudioRate] in=0 fco=0 res=0;    FILTER: TRUE
moogVCF :: UGen -> UGen -> UGen -> UGen
moogVCF in_ fco res = mkUGen Nothing [AudioRate] (Right [0]) "MoogVCF" [in_,fco,res] Nothing 1 (Special 0) NoId

-- | Stereo reverb
--
--  NHHall [AudioRate] in1=0.0 in2=0.0 rt60=1.0 stereo=0.5 lowFreq=200.0 lowRatio=0.5 hiFreq=4000.0 hiRatio=0.5 earlyDiffusion=0.5 lateDiffusion=0.5 modRate=0.2 modDepth=0.3
nhHall :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nhHall in1 in2 rt60 stereo lowFreq lowRatio hiFreq hiRatio earlyDiffusion lateDiffusion modRate modDepth = mkUGen Nothing [AudioRate] (Right [0,1]) "NHHall" [in1,in2,rt60,stereo,lowFreq,lowRatio,hiFreq,hiRatio,earlyDiffusion,lateDiffusion,modRate,modDepth] Nothing 2 (Special 0) NoId

-- | Non Linear Filter Equation
--
--  NL [AudioRate] input=0 bufnuma=0 bufnumb=1 guard1=1000 guard2=100
nl :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nl rate input bufnuma bufnumb guard1 guard2 = mkUGen Nothing [AudioRate] (Left rate) "NL" [input,bufnuma,bufnumb,guard1,guard2] Nothing 1 (Special 0) NoId

-- | Arbitrary Non Linear Filter Equation
--
--  NL2 [AudioRate] input=0 bufnum=0 maxsizea=10 maxsizeb=10 guard1=1000 guard2=100
nl2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nl2 rate input bufnum maxsizea maxsizeb guard1 guard2 = mkUGen Nothing [AudioRate] (Left rate) "NL2" [input,bufnum,maxsizea,maxsizeb,guard1,guard2] Nothing 1 (Special 0) NoId

-- | Non-linear Filter
--
--  NLFiltC [ControlRate,AudioRate] input=0 a=0 b=0 d=0 c=0 l=0
nlFiltC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nlFiltC rate input a b d c l = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "NLFiltC" [input,a,b,d,c,l] Nothing 1 (Special 0) NoId

-- | Non-linear Filter
--
--  NLFiltL [ControlRate,AudioRate] input=0 a=0 b=0 d=0 c=0 l=0
nlFiltL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nlFiltL rate input a b d c l = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "NLFiltL" [input,a,b,d,c,l] Nothing 1 (Special 0) NoId

-- | Non-linear Filter
--
--  NLFiltN [ControlRate,AudioRate] input=0 a=0 b=0 d=0 c=0 l=0
nlFiltN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nlFiltN rate input a b d c l = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "NLFiltN" [input,a,b,d,c,l] Nothing 1 (Special 0) NoId

-- | physical modeling simulation; N tubes
--
--  NTube [AudioRate] input=0 lossarray=1 karray=0 delaylengtharray=0
nTube :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
nTube rate input lossarray karray delaylengtharray = mkUGen Nothing [AudioRate] (Left rate) "NTube" [input,lossarray,karray,delaylengtharray] Nothing 1 (Special 0) NoId

-- | Find the nearest-neighbours in a set of points
--
--  NearestN [ControlRate] treebuf=0 in=0 gate=1 num=1
nearestN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
nearestN rate treebuf in_ gate_ num = mkUGen Nothing [ControlRate] (Left rate) "NearestN" [treebuf,in_,gate_,num] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  NeedleRect [AudioRate] rate=1 imgWidth=100 imgHeight=100 rectX=0 rectY=0 rectW=100 rectH=100
needleRect :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
needleRect rate rate_ imgWidth imgHeight rectX rectY rectW rectH = mkUGen Nothing [AudioRate] (Left rate) "NeedleRect" [rate_,imgWidth,imgHeight,rectX,rectY,rectW,rectH] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  NeoFormant [ControlRate,AudioRate] formantfreq=100 carrierfreq=200 phaseshift=0.5
neoFormant :: Rate -> UGen -> UGen -> UGen -> UGen
neoFormant rate formantfreq carrierfreq phaseshift = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "NeoFormant" [formantfreq,carrierfreq,phaseshift] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  NeoVarSawOsc [ControlRate,AudioRate] freq=100 pw=0.5 waveshape=0.5
neoVarSawOsc :: Rate -> UGen -> UGen -> UGen -> UGen
neoVarSawOsc rate freq pw waveshape = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "NeoVarSawOsc" [freq,pw,waveshape] Nothing 1 (Special 0) NoId

-- | APU Chip Sound Simulator
--
--  Nes2 [AudioRate] trig=0 a0=0 a1=0 a2=0 a3=0 b0=0 b1=0 b2=0 b3=0 c0=0 c2=0 c3=0 d0=0 d2=0 d3=0 e0=0 e1=0 e2=0 e3=0 smask=0
nes2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nes2 rate trig_ a0 a1 a2 a3 b0 b1 b2 b3 c0 c2 c3 d0 d2 d3 e0 e1 e2 e3 smask = mkUGen Nothing [AudioRate] (Left rate) "Nes2" [trig_,a0,a1,a2,a3,b0,b1,b2,b3,c0,c2,c3,d0,d2,d3,e0,e1,e2,e3,smask] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  NestedAllpassC [AudioRate] in=0 maxdelay1=0.036 delay1=0.036 gain1=0.08 maxdelay2=0.03 delay2=0.03 gain2=0.3;    FILTER: TRUE
nestedAllpassC :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nestedAllpassC in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 = mkUGen Nothing [AudioRate] (Right [0]) "NestedAllpassC" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  NestedAllpassL [AudioRate] in=0 maxdelay1=0.036 delay1=0.036 gain1=0.08 maxdelay2=0.03 delay2=0.03 gain2=0.3;    FILTER: TRUE
nestedAllpassL :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nestedAllpassL in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 = mkUGen Nothing [AudioRate] (Right [0]) "NestedAllpassL" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  NestedAllpassN [AudioRate] in=0 maxdelay1=0.036 delay1=0.036 gain1=0.08 maxdelay2=0.03 delay2=0.03 gain2=0.3;    FILTER: TRUE
nestedAllpassN :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
nestedAllpassN in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 = mkUGen Nothing [AudioRate] (Right [0]) "NestedAllpassN" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSFold4 [AudioRate] in=0 lo=0 hi=0
osFold4 :: Rate -> UGen -> UGen -> UGen -> UGen
osFold4 rate in_ lo hi = mkUGen Nothing [AudioRate] (Left rate) "OSFold4" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSFold8 [AudioRate] in=0 lo=0 hi=0
osFold8 :: Rate -> UGen -> UGen -> UGen -> UGen
osFold8 rate in_ lo hi = mkUGen Nothing [AudioRate] (Left rate) "OSFold8" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSTrunc4 [AudioRate] in=0 quant=0.5
osTrunc4 :: Rate -> UGen -> UGen -> UGen
osTrunc4 rate in_ quant = mkUGen Nothing [AudioRate] (Left rate) "OSTrunc4" [in_,quant] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSTrunc8 [AudioRate] in=0 quant=0.5
osTrunc8 :: Rate -> UGen -> UGen -> UGen
osTrunc8 rate in_ quant = mkUGen Nothing [AudioRate] (Left rate) "OSTrunc8" [in_,quant] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSWrap4 [AudioRate] in=0 lo=0 hi=0
osWrap4 :: Rate -> UGen -> UGen -> UGen -> UGen
osWrap4 rate in_ lo hi = mkUGen Nothing [AudioRate] (Left rate) "OSWrap4" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSWrap8 [AudioRate] in=0 lo=0 hi=0
osWrap8 :: Rate -> UGen -> UGen -> UGen -> UGen
osWrap8 rate in_ lo hi = mkUGen Nothing [AudioRate] (Left rate) "OSWrap8" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Extract basic statistics from a series of onset triggers
--
--  OnsetStatistics [ControlRate] input=0 windowsize=1 hopsize=0.1
onsetStatistics :: Rate -> UGen -> UGen -> UGen -> UGen
onsetStatistics rate input windowsize hopsize = mkUGen Nothing [ControlRate] (Left rate) "OnsetStatistics" [input,windowsize,hopsize] Nothing 3 (Special 0) NoId

-- | Chemical reaction modelling Oscillator
--
--  Oregonator [AudioRate] reset=0 rate=0.01 epsilon=1 mu=1 q=1 initx=0.5 inity=0.5 initz=0.5
oregonator :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
oregonator rate reset rate_ epsilon mu q initx inity initz = mkUGen Nothing [AudioRate] (Left rate) "Oregonator" [reset,rate_,epsilon,mu,q,initx,inity,initz] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  OscBank [ControlRate,AudioRate] freq=100 gain=1 saw8=0.5 square8=0.5 saw4=0.5 square4=0.5 saw2=0.5 square2=0.5 saw1=0.5
oscBank :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
oscBank rate freq gain saw8 square8 saw4 square4 saw2 square2 saw1 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "OscBank" [freq,gain,saw8,square8,saw4,square4,saw2,square2,saw1] Nothing 1 (Special 0) NoId

-- | Piano physical model.
--
--  OteyPiano [AudioRate] freq=440 vel=1 t_gate=0 rmin=0.35 rmax=2 rampl=4 rampr=8 rcore=1 lmin=0.07 lmax=1.4 lampl=-4 lampr=4 rho=1 e=1 zb=1 zh=0 mh=1 k=0.2 alpha=1 p=1 hpos=0.142 loss=1 detune=0.0003 hammer_type=1
oteyPiano :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
oteyPiano rate freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type = mkUGen Nothing [AudioRate] (Left rate) "OteyPiano" [freq,vel,t_gate,rmin,rmax,rampl,rampr,rcore,lmin,lmax,lampl,lampr,rho,e,zb,zh,mh,k,alpha,p,hpos,loss,detune,hammer_type] Nothing 1 (Special 0) NoId

-- | Piano physical model.
--
--  OteyPianoStrings [AudioRate] freq=440 vel=1 t_gate=0 rmin=0.35 rmax=2 rampl=4 rampr=8 rcore=1 lmin=0.07 lmax=1.4 lampl=-4 lampr=4 rho=1 e=1 zb=1 zh=0 mh=1 k=0.2 alpha=1 p=1 hpos=0.142 loss=1 detune=0.0003 hammer_type=1
oteyPianoStrings :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
oteyPianoStrings rate freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type = mkUGen Nothing [AudioRate] (Left rate) "OteyPianoStrings" [freq,vel,t_gate,rmin,rmax,rampl,rampr,rcore,lmin,lmax,lampl,lampr,rho,e,zb,zh,mh,k,alpha,p,hpos,loss,detune,hammer_type] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OteySoundBoard [AudioRate] inp=0 c1=20 c3=20 mix=0.8;    FILTER: TRUE
oteySoundBoard :: UGen -> UGen -> UGen -> UGen -> UGen
oteySoundBoard inp c1 c3 mix = mkUGen Nothing [AudioRate] (Right [0]) "OteySoundBoard" [inp,c1,c3,mix] Nothing 1 (Special 0) NoId

-- | Return mag and freq data from a CSound pv
--
--  PVInfo [ControlRate,AudioRate] pvbuffer=0 binNum=0 filePointer=0
pvInfo :: Rate -> UGen -> UGen -> UGen -> UGen
pvInfo rate pvbuffer binNum filePointer = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "PVInfo" [pvbuffer,binNum,filePointer] Nothing 2 (Special 0) NoId

-- | Resynthesize Csound PV data
--
--  PVSynth [AudioRate] pvbuffer=0 numBins=0 binStart=0 binSkip=1 filePointer=0 freqMul=1 freqAdd=0
pvSynth :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pvSynth rate pvbuffer numBins binStart binSkip filePointer freqMul freqAdd = mkUGen Nothing [AudioRate] (Left rate) "PVSynth" [pvbuffer,numBins,binStart,binSkip,filePointer,freqMul,freqAdd] Nothing 1 (Special 0) NoId

-- | Plays FFT data to a memory buffer
--
--  PV_BinBufRd [ControlRate] buffer=0 playbuf=0 point=1 binStart=0 binSkip=1 numBins=1 clear=0
pv_BinBufRd :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinBufRd buffer playbuf_ point binStart binSkip numBins clear = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_BinBufRd" [buffer,playbuf_,point,binStart,binSkip,numBins,clear] Nothing 1 (Special 0) NoId

-- | Delay and Feedback on a bin by bin basis.
--
--  PV_BinDelay [ControlRate] buffer=0 maxdelay=0 delaybuf=0 fbbuf=0 hop=0.5
pv_BinDelay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinDelay buffer maxdelay delaybuf fbbuf hop = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_BinDelay" [buffer,maxdelay,delaybuf,fbbuf,hop] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_BinFilter [ControlRate] buffer=0 start=0 end=0
pv_BinFilter :: UGen -> UGen -> UGen -> UGen
pv_BinFilter buffer start end = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_BinFilter" [buffer,start,end] Nothing 1 (Special 0) NoId

-- | Plays FFT data to a memory buffer
--
--  PV_BinPlayBuf [ControlRate] buffer=0 playbuf=0 rate=1 offset=0 binStart=0 binSkip=1 numBins=1 loop=0 clear=0
pv_BinPlayBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinPlayBuf buffer playbuf_ rate_ offset binStart binSkip numBins loop clear = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_BinPlayBuf" [buffer,playbuf_,rate_,offset,binStart,binSkip,numBins,loop,clear] Nothing 1 (Special 0) NoId

-- | Plays FFT data from a memory buffer
--
--  PV_BufRd [ControlRate] buffer=0 playbuf=0 point=1
pv_BufRd :: UGen -> UGen -> UGen -> UGen
pv_BufRd buffer playbuf_ point = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_BufRd" [buffer,playbuf_,point] Nothing 1 (Special 0) NoId

-- | returns common magnitudes
--
--  PV_CommonMag [ControlRate] bufferA=0 bufferB=0 tolerance=0 remove=0
pv_CommonMag :: UGen -> UGen -> UGen -> UGen -> UGen
pv_CommonMag bufferA bufferB tolerance remove = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_CommonMag" [bufferA,bufferB,tolerance,remove] Nothing 1 (Special 0) NoId

-- | multiplies common magnitudes
--
--  PV_CommonMul [ControlRate] bufferA=0 bufferB=0 tolerance=0 remove=0
pv_CommonMul :: UGen -> UGen -> UGen -> UGen -> UGen
pv_CommonMul bufferA bufferB tolerance remove = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_CommonMul" [bufferA,bufferB,tolerance,remove] Nothing 1 (Special 0) NoId

-- | simple spectral compression/expansion
--
--  PV_Compander [ControlRate] buffer=0 thresh=50 slopeBelow=1 slopeAbove=1
pv_Compander :: UGen -> UGen -> UGen -> UGen -> UGen
pv_Compander buffer thresh slopeBelow slopeAbove = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Compander" [buffer,thresh,slopeBelow,slopeAbove] Nothing 1 (Special 0) NoId

-- | zero bins with interpolation
--
--  PV_Cutoff [ControlRate] bufferA=0 bufferB=0 wipe=0
pv_Cutoff :: UGen -> UGen -> UGen -> UGen
pv_Cutoff bufferA bufferB wipe = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Cutoff" [bufferA,bufferB,wipe] Nothing 1 (Special 0) NoId

-- | Return the even numbered bins in an FFT buffer
--
--  PV_EvenBin [ControlRate] buffer=0
pv_EvenBin :: UGen -> UGen
pv_EvenBin buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_EvenBin" [buffer] Nothing 1 (Special 0) NoId

-- | extract a repeating loop out from audio
--
--  PV_ExtractRepeat [ControlRate] buffer=0 loopbuf=0 loopdur=0 memorytime=30 which=0 ffthop=0.5 thresh=1
pv_ExtractRepeat :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_ExtractRepeat buffer loopbuf_ loopdur memorytime which ffthop thresh = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_ExtractRepeat" [buffer,loopbuf_,loopdur,memorytime,which,ffthop,thresh] Nothing 1 (Special 0) NoId

-- | Freeze FFT frames
--
--  PV_Freeze [ControlRate] buffer=0 freeze=0
pv_Freeze :: UGen -> UGen -> UGen
pv_Freeze buffer freeze = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Freeze" [buffer,freeze] Nothing 1 (Special 0) NoId

-- | Store FFT data in another buffer for other use
--
--  PV_FreqBuffer [ControlRate] buffer=0 databuffer=0
pv_FreqBuffer :: UGen -> UGen -> UGen
pv_FreqBuffer buffer databuffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_FreqBuffer" [buffer,databuffer] Nothing 1 (Special 0) NoId

-- | Invert FFT frames
--
--  PV_Invert [ControlRate] buffer=0
pv_Invert :: UGen -> UGen
pv_Invert buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Invert" [buffer] Nothing 1 (Special 0) NoId

-- | Store FFT data in another buffer for other use
--
--  PV_MagBuffer [ControlRate] buffer=0 databuffer=0
pv_MagBuffer :: UGen -> UGen -> UGen
pv_MagBuffer buffer databuffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagBuffer" [buffer,databuffer] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagExp [ControlRate] buffer=0
pv_MagExp :: UGen -> UGen
pv_MagExp buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagExp" [buffer] Nothing 1 (Special 0) NoId

-- | reduces magnitudes above or below thresh
--
--  PV_MagGate [ControlRate] buffer=0 thresh=1 remove=0
pv_MagGate :: UGen -> UGen -> UGen -> UGen
pv_MagGate buffer thresh remove = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagGate" [buffer,thresh,remove] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagLog [ControlRate] buffer=0
pv_MagLog :: UGen -> UGen
pv_MagLog buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagLog" [buffer] Nothing 1 (Special 0) NoId

-- | Remap magnitudes to a new mag curve
--
--  PV_MagMap [ControlRate] buffer=0 mapbuf=0
pv_MagMap :: UGen -> UGen -> UGen
pv_MagMap buffer mapbuf = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagMap" [buffer,mapbuf] Nothing 1 (Special 0) NoId

-- | subtract spectral energy
--
--  PV_MagMinus [ControlRate] bufferA=0 bufferB=0 remove=1
pv_MagMinus :: UGen -> UGen -> UGen -> UGen
pv_MagMinus bufferA bufferB remove = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagMinus" [bufferA,bufferB,remove] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagMulAdd [ControlRate] buffer=0
pv_MagMulAdd :: UGen -> UGen
pv_MagMulAdd buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagMulAdd" [buffer] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagScale [ControlRate] bufferA=0 bufferB=0
pv_MagScale :: UGen -> UGen -> UGen
pv_MagScale bufferA bufferB = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagScale" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Smooth spectral magnitudes over time
--
--  PV_MagSmooth [ControlRate] buffer=0 factor=0.1
pv_MagSmooth :: UGen -> UGen -> UGen
pv_MagSmooth buffer factor = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagSmooth" [buffer,factor] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagSubtract [ControlRate] bufferA=0 bufferB=0 zerolimit=0
pv_MagSubtract :: UGen -> UGen -> UGen -> UGen
pv_MagSubtract bufferA bufferB zerolimit = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MagSubtract" [bufferA,bufferB,zerolimit] Nothing 1 (Special 0) NoId

-- | Return the N strongest bins
--
--  PV_MaxMagN [ControlRate] buffer=0 numbins=0
pv_MaxMagN :: UGen -> UGen -> UGen
pv_MaxMagN buffer numbins = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MaxMagN" [buffer,numbins] Nothing 1 (Special 0) NoId

-- | Return the N weakest bins
--
--  PV_MinMagN [ControlRate] buffer=0 numbins=0
pv_MinMagN :: UGen -> UGen -> UGen
pv_MinMagN buffer numbins = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_MinMagN" [buffer,numbins] Nothing 1 (Special 0) NoId

-- | one kind of spectral morphing
--
--  PV_Morph [ControlRate] bufferA=0 bufferB=0 morph=0
pv_Morph :: UGen -> UGen -> UGen -> UGen
pv_Morph bufferA bufferB morph = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Morph" [bufferA,bufferB,morph] Nothing 1 (Special 0) NoId

-- | Return only bins that are unstable
--
--  PV_NoiseSynthF [ControlRate] buffer=0 threshold=0.1 numFrames=2 initflag=0
pv_NoiseSynthF :: UGen -> UGen -> UGen -> UGen -> UGen
pv_NoiseSynthF buffer threshold numFrames initflag = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_NoiseSynthF" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | Return only bins that are unstable
--
--  PV_NoiseSynthP [ControlRate] buffer=0 threshold=0.1 numFrames=2 initflag=0
pv_NoiseSynthP :: UGen -> UGen -> UGen -> UGen -> UGen
pv_NoiseSynthP buffer threshold numFrames initflag = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_NoiseSynthP" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | Return the odd numbered bins in an FFT buffer
--
--  PV_OddBin [ControlRate] buffer=0
pv_OddBin :: UGen -> UGen
pv_OddBin buffer = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_OddBin" [buffer] Nothing 1 (Special 0) NoId

-- | Return only bins that are stable
--
--  PV_PartialSynthF [ControlRate] buffer=0 threshold=0.1 numFrames=2 initflag=0
pv_PartialSynthF :: UGen -> UGen -> UGen -> UGen -> UGen
pv_PartialSynthF buffer threshold numFrames initflag = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_PartialSynthF" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | Return only bins that are stable
--
--  PV_PartialSynthP [ControlRate] buffer=0 threshold=0.1 numFrames=2 initflag=0
pv_PartialSynthP :: UGen -> UGen -> UGen -> UGen -> UGen
pv_PartialSynthP buffer threshold numFrames initflag = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_PartialSynthP" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_PitchShift [ControlRate] buffer=0 ratio=0
pv_PitchShift :: UGen -> UGen -> UGen
pv_PitchShift buffer ratio = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_PitchShift" [buffer,ratio] Nothing 1 (Special 0) NoId

-- | Plays FFT data to a memory buffer
--
--  PV_PlayBuf [ControlRate] buffer=0 playbuf=0 rate=1 offset=0 loop=0
pv_PlayBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_PlayBuf buffer playbuf_ rate_ offset loop = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_PlayBuf" [buffer,playbuf_,rate_,offset,loop] Nothing 1 (Special 0) NoId

-- | Records FFT data to a memory buffer
--
--  PV_RecordBuf [ControlRate] buffer=0 recbuf=0 offset=0 run=0 loop=0 hop=0.5 wintype=0
pv_RecordBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RecordBuf buffer recbuf offset run loop hop wintype = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_RecordBuf" [buffer,recbuf,offset,run,loop,hop,wintype] Nothing 1 (Special 0) NoId

-- | combine low and high bins from two inputs with interpolation
--
--  PV_SoftWipe [ControlRate] bufferA=0 bufferB=0 wipe=0
pv_SoftWipe :: UGen -> UGen -> UGen -> UGen
pv_SoftWipe bufferA bufferB wipe = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_SoftWipe" [bufferA,bufferB,wipe] Nothing 1 (Special 0) NoId

-- | A harmonic enhancer
--
--  PV_SpectralEnhance [ControlRate] buffer=0 numPartials=8 ratio=2 strength=0.1
pv_SpectralEnhance :: UGen -> UGen -> UGen -> UGen -> UGen
pv_SpectralEnhance buffer numPartials ratio strength = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_SpectralEnhance" [buffer,numPartials,ratio,strength] Nothing 1 (Special 0) NoId

-- | Maps the spectral envelope of one FFT process onto another
--
--  PV_SpectralMap [ControlRate] buffer=0 specBuffer=0 floor=0 freeze=0 mode=0 norm=0 window=0
pv_SpectralMap :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_SpectralMap buffer specBuffer floor_ freeze mode norm window = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_SpectralMap" [buffer,specBuffer,floor_,freeze,mode,norm,window] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_Split [ControlRate] bufferA=0 bufferB=0
pv_Split :: UGen -> UGen -> UGen
pv_Split bufferA bufferB = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Split" [bufferA,bufferB] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_Whiten [ControlRate] chain=0 trackbufnum=0 relaxtime=2 floor=0.1 smear=0 bindownsample=0
pv_Whiten :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_Whiten chain trackbufnum relaxtime floor_ smear bindownsample = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_Whiten" [chain,trackbufnum,relaxtime,floor_,smear,bindownsample] Nothing 1 (Special 0) NoId

-- | one kind of spectral morphing
--
--  PV_XFade [ControlRate] bufferA=0 bufferB=0 fade=0
pv_xFade :: UGen -> UGen -> UGen -> UGen
pv_xFade bufferA bufferB fade = mkUGen Nothing [ControlRate] (Left ControlRate) "PV_XFade" [bufferA,bufferB,fade] Nothing 1 (Special 0) NoId

-- | Equal power pan across an array of speakers
--
--  PanX [ControlRate,AudioRate] numChans=0 in=0 pos=0 level=1 width=2
panX :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
panX rate numChans in_ pos level width = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "PanX" [numChans,in_,pos,level,width] Nothing 0 (Special 0) NoId

-- | (Undocumented class)
--
--  PanX2D [ControlRate,AudioRate] numChansX=0 numChansY=0 in=0 posX=0 posY=0 level=1 widthX=2 widthY=2
panX2D :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
panX2D rate numChansX numChansY in_ posX posY level widthX widthY = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "PanX2D" [numChansX,numChansY,in_,posX,posY,level,widthX,widthY] Nothing 0 (Special 0) NoId

-- | (Undocumented class)
--
--  PeakEQ2 [AudioRate] in=0 freq=1200 rs=1 db=0
peakEQ2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
peakEQ2 rate in_ freq rs db = mkUGen Nothing [AudioRate] (Left rate) "PeakEQ2" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PeakEQ4 [AudioRate] in=0 freq=1200 rs=1 db=0
peakEQ4 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
peakEQ4 rate in_ freq rs db = mkUGen Nothing [AudioRate] (Left rate) "PeakEQ4" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | 3D Perlin Noise
--
--  Perlin3 [ControlRate,AudioRate] x=0 y=0 z=0
perlin3 :: Rate -> UGen -> UGen -> UGen -> UGen
perlin3 rate x y z = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Perlin3" [x,y,z] Nothing 1 (Special 0) NoId

-- | Sample permutation UGen.
--
--  PermMod [AudioRate] in=0 freq=100
permMod :: Rate -> UGen -> UGen -> UGen
permMod rate in_ freq = mkUGen Nothing [AudioRate] (Left rate) "PermMod" [in_,freq] Nothing 1 (Special 0) NoId

-- | Sample permutation UGen with programmable pattern.
--
--  PermModArray [AudioRate] in=0 freq=50 pattern=0
permModArray :: Rate -> UGen -> UGen -> UGen -> UGen
permModArray rate in_ freq pattern_ = mkUGen Nothing [AudioRate] (Left rate) "PermModArray" [in_,freq,pattern_] Nothing 1 (Special 0) NoId

-- | Sample permutation UGen with tail.
--
--  PermModT [AudioRate] in=0 outfreq=440 infreq=5000
permModT :: Rate -> UGen -> UGen -> UGen -> UGen
permModT rate in_ outfreq infreq = mkUGen Nothing [AudioRate] (Left rate) "PermModT" [in_,outfreq,infreq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PhasorModal [AudioRate] input=0 freq=100 decay=0.25 damp=1 amp=0.5 phase=0;    FILTER: TRUE
phasorModal :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
phasorModal input freq decay_ damp amp phase = mkUGen Nothing [AudioRate] (Right [0]) "PhasorModal" [input,freq,decay_,damp,amp,phase] Nothing 1 (Special 0) NoId

-- | Tree classifier using (hyper)planes – UGen or language-side
--
--  PlaneTree [ControlRate] treebuf=0 in=0 gate=1
planeTree :: Rate -> UGen -> UGen -> UGen -> UGen
planeTree rate treebuf in_ gate_ = mkUGen Nothing [ControlRate] (Left rate) "PlaneTree" [treebuf,in_,gate_] Nothing 1 (Special 0) NoId

-- | POKEY Chip Sound Simulator
--
--  Pokey [AudioRate] audf1=0 audc1=0 audf2=0 audc2=0 audf3=0 audc3=0 audf4=0 audc4=0 audctl=0
pokey :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pokey rate audf1 audc1 audf2 audc2 audf3 audc3 audf4 audc4 audctl = mkUGen Nothing [AudioRate] (Left rate) "Pokey" [audf1,audc1,audf2,audc2,audf3,audc3,audf4,audc4,audctl] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PosRatio [AudioRate] in=0 period=100 thresh=0.1
posRatio :: Rate -> UGen -> UGen -> UGen -> UGen
posRatio rate in_ period thresh = mkUGen Nothing [AudioRate] (Left rate) "PosRatio" [in_,period,thresh] Nothing 1 (Special 0) NoId

-- | debug assistance
--
--  PrintVal [ControlRate] in=0 numblocks=100 id=0
printVal :: Rate -> UGen -> UGen -> UGen -> UGen
printVal rate in_ numblocks id_ = mkUGen Nothing [ControlRate] (Left rate) "PrintVal" [in_,numblocks,id_] Nothing 1 (Special 0) NoId

-- | constant Q transform pitch follower
--
--  Qitch [ControlRate] in=0 databufnum=0 ampThreshold=0.01 algoflag=1 ampbufnum=0 minfreq=0 maxfreq=2500
qitch :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
qitch rate in_ databufnum ampThreshold algoflag ampbufnum minfreq maxfreq = mkUGen Nothing [ControlRate] (Left rate) "Qitch" [in_,databufnum,ampThreshold,algoflag,ampbufnum,minfreq,maxfreq] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  Bezier [ControlRate,AudioRate] haltAfter=100 dx=0.0001 freq=440 phase=0 *param=0;    MCE=1
bezier :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
bezier rate haltAfter dx freq phase param = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Bezier" [haltAfter,dx,freq,phase] (Just [param]) 1 (Special 0) NoId

-- | rotating clock divider
--
--  RCD [AudioRate] clock=0 rotate=0 reset=0 div=0 spread=0 auto=0 len=0 down=0 gates=0;    FILTER: TRUE
rcd :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rcd clock rotate_ reset div_ spread auto len down gates = mkUGen Nothing [AudioRate] (Right [0]) "RCD" [clock,rotate_,reset,div_,spread,auto,len,down,gates] Nothing 8 (Special 0) NoId

-- | (Undocumented class)
--
--  RDL [AudioRate] numChannels=1 inputArray=0
rdl :: Rate -> UGen -> UGen -> UGen
rdl rate numChannels inputArray = mkUGen Nothing [AudioRate] (Left rate) "RDL" [numChannels,inputArray] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  DX7 [AudioRate] bufnum=0 on=0 off=0 data=0 vc=0 mnn=60 vel=99 pw=0 mw=0 bc=0 fc=0
dx7 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dx7 rate bufnum on off data_ vc mnn vel pw mw bc fc = mkUGen Nothing [AudioRate] (Left rate) "DX7" [bufnum,on,off,data_,vc,mnn,vel,pw,mw,bc,fc] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RDX7Env [AudioRate] gate=0 data=0 r1=0 r2=0 r3=0 r4=0 l1=0 l2=0 l3=0 l4=0 ol=0
rdx7Env :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rdx7Env rate gate_ data_ r1 r2 r3 r4 l1 l2 l3 l4 ol = mkUGen Nothing [AudioRate] (Left rate) "RDX7Env" [gate_,data_,r1,r2,r3,r4,l1,l2,l3,l4,ol] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RDelayMap [AudioRate] bufnum=0 in=0 dynamic=0 *spec=0;    MCE=1, FILTER: TRUE
rDelayMap :: UGen -> UGen -> UGen -> UGen -> UGen
rDelayMap bufnum in_ dynamic spec = mkUGen Nothing [AudioRate] (Right [1]) "RDelayMap" [bufnum,in_,dynamic] (Just [spec]) 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RDelaySet [AudioRate] input=0 *setArray=0;    MCE=1, FILTER: TRUE
rDelaySet :: UGen -> UGen -> UGen
rDelaySet input setArray = mkUGen Nothing [AudioRate] (Right [0]) "RDelaySet" [input] (Just [setArray]) 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RDelaySetBuf [AudioRate] bufnum=0 in=0 spec=0
rDelaySetBuf :: Rate -> UGen -> UGen -> UGen -> UGen
rDelaySetBuf rate bufnum in_ spec = mkUGen Nothing [AudioRate] (Left rate) "RDelaySetBuf" [bufnum,in_,spec] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  dustRange [AudioRate] iot_min=0.1 iot_max=1
dustRangeId :: ID a => a -> Rate -> UGen -> UGen -> UGen
dustRangeId z rate iot_min iot_max = mkUGen Nothing [AudioRate] (Left rate) "DustRange" [iot_min,iot_max] Nothing 1 (Special 0) (toUId z)

-- | (Undocumented class)
--
--  ExpRandN [InitialisationRate] lo=0 hi=1;    NC INPUT: True, NONDET
expRandNId :: ID a => Int -> a -> UGen -> UGen -> UGen
expRandNId numChannels z lo hi = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "ExpRandN" [lo,hi] Nothing numChannels (Special 0) (toUId z)

-- | Monad variant of ExpRandN.
expRandNM :: UId m => Int -> UGen -> UGen -> m UGen
expRandNM nc = liftUId2 (expRandNId nc)

-- | Unsafe variant of ExpRandN.
expRandN ::  Int -> UGen -> UGen -> UGen
expRandN nc = liftUnsafe2 (expRandNM nc)

-- | (Undocumented class)
--
--  Freezer [AudioRate] bufnum=0 left=0 right=1 gain=1 increment=1 incrementOffset=0 incrementRandom=0 rightRandom=0 syncPhaseTrigger=0 randomizePhaseTrigger=0 numberOfLoops=4
freezer :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
freezer bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops = mkUGen Nothing [AudioRate] (Left AudioRate) "Freezer" [bufnum,left,right,gain,increment,incrementOffset,incrementRandom,rightRandom,syncPhaseTrigger,randomizePhaseTrigger,numberOfLoops] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  IRandN [] numChannels=2 lo=0 hi=127
iRandNId :: ID a => a -> Int -> UGen -> UGen -> UGen
iRandNId z numChannels lo hi = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "IRandN" [lo,hi] Nothing numChannels (Special 0) (toUId z)

-- | Monad variant of IRandN.
iRandNM :: UId m => Int -> UGen -> UGen -> m UGen
iRandNM nc = liftUId2 (iRandNId nc)

-- | Unsafe variant of IRandN.
iRandN ::  Int -> UGen -> UGen -> UGen
iRandN nc = liftUnsafe2 (iRandNM nc)

-- | TB303 Filter Emulation
--
--  RLPFD [ControlRate,AudioRate] in=0 ffreq=440 res=0 dist=0;    FILTER: TRUE
rlpfd :: UGen -> UGen -> UGen -> UGen -> UGen
rlpfd in_ ffreq res dist = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "RLPFD" [in_,ffreq,res,dist] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RLagC [ControlRate] in=0 timeUp=0.1 curveUp=0 timeDown=0.1 curveDown=0;    FILTER: TRUE
rLagC :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rLagC in_ timeUp curveUp timeDown curveDown = mkUGen Nothing [ControlRate] (Right [0]) "RLagC" [in_,timeUp,curveUp,timeDown,curveDown] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LinRandN [InitialisationRate] lo=0 hi=1 minmax=0;    NC INPUT: True, NONDET
linRandNId :: ID a => Int -> a -> UGen -> UGen -> UGen -> UGen
linRandNId numChannels z lo hi minmax = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "LinRandN" [lo,hi,minmax] Nothing numChannels (Special 0) (toUId z)

-- | Monad variant of LinRandN.
linRandNM :: UId m => Int -> UGen -> UGen -> UGen -> m UGen
linRandNM nc = liftUId3 (linRandNId nc)

-- | Unsafe variant of LinRandN.
linRandN ::  Int -> UGen -> UGen -> UGen -> UGen
linRandN nc = liftUnsafe3 (linRandNM nc)

-- | (Undocumented class)
--
--  RLoopSet [AudioRate] bufnum=0 left=0 right=1 gain=1 increment=1 spec=0
rLoopSet :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rLoopSet rate bufnum left right gain increment spec = mkUGen Nothing [AudioRate] (Left rate) "RLoopSet" [bufnum,left,right,gain,increment,spec] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMAFoodChainL [AudioRate] freq=22050 a1=5 b1=3 d1=0.4 a2=0.1 b2=2 d2=0.01 k=1.0943 r=0.8904 h=0.05 xi=0.1 yi=0 zi=0
rmaFoodChainL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rmaFoodChainL rate freq a1 b1 d1 a2 b2 d2 k r h xi yi zi = mkUGen Nothing [AudioRate] (Left rate) "RMAFoodChainL" [freq,a1,b1,d1,a2,b2,d2,k,r,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  RMEQ [AudioRate] in=0 freq=440 rq=0.1 k=0;    FILTER: TRUE
rmeq :: UGen -> UGen -> UGen -> UGen -> UGen
rmeq in_ freq rq k = mkUGen Nothing [AudioRate] (Right [0]) "RMEQ" [in_,freq,rq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMEQSuite [] maxSize=0
rmeqSuite :: Rate -> UGen -> UGen
rmeqSuite rate maxSize = mkUGen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "RMEQSuite" [maxSize] Nothing 1 (Special 0) NoId

-- | root mean square
--
--  RMS [ControlRate,AudioRate] in=0 lpFreq=10
rms :: Rate -> UGen -> UGen -> UGen
rms rate in_ lpFreq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "RMS" [in_,lpFreq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMShelf [AudioRate] in=0 freq=440 k=0
rmShelf :: Rate -> UGen -> UGen -> UGen -> UGen
rmShelf rate in_ freq k = mkUGen Nothing [AudioRate] (Left rate) "RMShelf" [in_,freq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMShelf2 [AudioRate] in=0 freq=440 k=0
rmShelf2 :: Rate -> UGen -> UGen -> UGen -> UGen
rmShelf2 rate in_ freq k = mkUGen Nothing [AudioRate] (Left rate) "RMShelf2" [in_,freq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  ObxdFilter [AudioRate] in=0 cutoff=440 resonance=0 multimode=0.5 bandpass=0 fourpole=0
obxdFilter :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
obxdFilter in_ cutoff resonance multimode bandpass fourpole = mkUGen Nothing [AudioRate] (Right [0]) "ObxdFilter" [in_,cutoff,resonance,multimode,bandpass,fourpole] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RPVDecayTbl [] fft_buf=0 decay_rate_buf=0 history_buf=0
rpvDecayTbl :: UGen -> UGen -> UGen -> UGen
rpvDecayTbl fft_buf decay_rate_buf history_buf = mkUGen Nothing [ControlRate] (Left ControlRate) "RPVDecayTbl" [fft_buf,decay_rate_buf,history_buf] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RandN [InitialisationRate] lo=0 hi=1;    NC INPUT: True, NONDET
randNId :: ID a => Int -> a -> UGen -> UGen -> UGen
randNId numChannels z lo hi = mkUGen Nothing [InitialisationRate] (Left InitialisationRate) "RandN" [lo,hi] Nothing numChannels (Special 0) (toUId z)

-- | Monad variant of RandN.
randNM :: UId m => Int -> UGen -> UGen -> m UGen
randNM nc = liftUId2 (randNId nc)

-- | Unsafe variant of RandN.
randN ::  Int -> UGen -> UGen -> UGen
randN nc = liftUnsafe2 (randNM nc)

-- | (Undocumented class)
--
--  RSVFBP [AudioRate] in=0 freq=440 q=0
svfBp :: UGen -> UGen -> UGen -> UGen
svfBp in_ freq q = mkUGen Nothing [AudioRate] (Right [0]) "SvfBp" [in_,freq,q] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SvfHp [AudioRate] in=0 freq=440 q=0
svfHp :: UGen -> UGen -> UGen -> UGen
svfHp in_ freq q = mkUGen Nothing [AudioRate] (Right [0]) "SvfHp" [in_,freq,q] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SvflP [AudioRate] in=0 freq=440 q=0
svfLp :: UGen -> UGen -> UGen -> UGen
svfLp in_ freq q = mkUGen Nothing [AudioRate] (Right [0]) "SvfLp" [in_,freq,q] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  ShufflerB [AudioRate] bufnum=0 readLocationMinima=0.01 readLocationMaxima=0.02 readIncrementMinima=1 readIncrementMaxima=1 durationMinima=0.2 durationMaxima=0.2 envelopeAmplitudeMinima=0.5 envelopeAmplitudeMaxima=0.5 envelopeShapeMinima=0.5 envelopeShapeMaxima=0.5 envelopeSkewMinima=0.5 envelopeSkewMaxima=0.5 stereoLocationMinima=0.5 stereoLocationMaxima=0.5 interOffsetTimeMinima=0.05 interOffsetTimeMaxima=0.01 ftableReadLocationIncrement=1 readIncrementQuanta=0 interOffsetTimeQuanta=0
shufflerB :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
shufflerB bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta = mkUGen Nothing [AudioRate] (Left AudioRate) "ShufflerB" [bufnum,readLocationMinima,readLocationMaxima,readIncrementMinima,readIncrementMaxima,durationMinima,durationMaxima,envelopeAmplitudeMinima,envelopeAmplitudeMaxima,envelopeShapeMinima,envelopeShapeMaxima,envelopeSkewMinima,envelopeSkewMaxima,stereoLocationMinima,stereoLocationMaxima,interOffsetTimeMinima,interOffsetTimeMaxima,ftableReadLocationIncrement,readIncrementQuanta,interOffsetTimeQuanta] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  RShufflerL [AudioRate] in=0 fragmentSize=0.01 maxDelay=0.01
rShufflerL :: UGen -> UGen -> UGen -> UGen
rShufflerL in_ fragmentSize maxDelay = mkUGen Nothing [AudioRate] (Right [0]) "RShufflerL" [in_,fragmentSize,maxDelay] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RSmplrIndex [ControlRate] buf=0 size=0 mnn=60
rSmplrIndex :: Rate -> UGen -> UGen -> UGen -> UGen
rSmplrIndex rate buf size mnn = mkUGen Nothing [ControlRate] (Left rate) "RSmplrIndex" [buf,size,mnn] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  TExpRandN [ControlRate] lo=0 hi=1 trigger=0;    NC INPUT: True, FILTER: TRUE, NONDET
tExpRandNId :: ID a => Int -> a -> UGen -> UGen -> UGen -> UGen
tExpRandNId numChannels z lo hi trigger = mkUGen Nothing [ControlRate] (Right [2]) "TExpRandN" [lo,hi,trigger] Nothing numChannels (Special 0) (toUId z)

-- | Monad variant of TExpRandN.
tExpRandNM :: UId m => Int -> UGen -> UGen -> UGen -> m UGen
tExpRandNM nc = liftUId3 (tExpRandNId nc)

-- | Unsafe variant of TExpRandN.
tExpRandN ::  Int -> UGen -> UGen -> UGen -> UGen
tExpRandN nc = liftUnsafe3 (tExpRandNM nc)

-- | (Undocumented class)
--
--  TLinRandN [ControlRate] lo=0 hi=1 minmax=0 trigger=0;    NC INPUT: True, FILTER: TRUE, NONDET
tLinRandNId :: ID a => Int -> a -> UGen -> UGen -> UGen -> UGen -> UGen
tLinRandNId numChannels z lo hi minmax trigger = mkUGen Nothing [ControlRate] (Right [3]) "TLinRandN" [lo,hi,minmax,trigger] Nothing numChannels (Special 0) (toUId z)

-- | Monad variant of TLinRandN.
tLinRandNM :: UId m => Int -> UGen -> UGen -> UGen -> UGen -> m UGen
tLinRandNM nc = liftUId4 (tLinRandNId nc)

-- | Unsafe variant of TLinRandN.
tLinRandN ::  Int -> UGen -> UGen -> UGen -> UGen -> UGen
tLinRandN nc = liftUnsafe4 (tLinRandNM nc)

-- | (Undocumented class)
--
--  TRandN [ControlRate] lo=0 hi=1 trigger=0;    NC INPUT: True, FILTER: TRUE, NONDET
tRandNId :: ID a => Int -> a -> UGen -> UGen -> UGen -> UGen
tRandNId numChannels z lo hi trigger = mkUGen Nothing [ControlRate] (Right [2]) "TRandN" [lo,hi,trigger] Nothing numChannels (Special 0) (toUId z)

-- | Monad variant of TRandN.
tRandNM :: UId m => Int -> UGen -> UGen -> UGen -> m UGen
tRandNM nc = liftUId3 (tRandNId nc)

-- | Unsafe variant of TRandN.
tRandN ::  Int -> UGen -> UGen -> UGen -> UGen
tRandN nc = liftUnsafe3 (tRandNM nc)

-- | (Undocumented class)
--
--  TScramble [InitialisationRate,ControlRate] trigger=0 *inputs=0;    MCE=1, FILTER: TRUE, NONDET
tScrambleId :: ID a => a -> UGen -> UGen -> UGen
tScrambleId z trigger inputs = mkUGen Nothing [InitialisationRate,ControlRate] (Right [0]) "TScramble" [trigger] (Just [inputs]) (length (mceChannels inputs) + 0) (Special 0) (toUId z)

-- | Monad variant of TScramble.
tScrambleM :: UId m => UGen -> UGen -> m UGen
tScrambleM = liftUId2 tScrambleId

-- | Unsafe variant of TScramble.
tScramble ::  UGen -> UGen -> UGen
tScramble = liftUnsafe2 tScrambleM

-- | (Undocumented class)
--
--  RTracePlay [ControlRate,AudioRate] bufnum=0 degree=4 rate=0 axis=1
rTracePlay :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
rTracePlay rate bufnum degree rate_ axis = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "RTracePlay" [bufnum,degree,rate_,axis] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RTraceRd [ControlRate,AudioRate] bufnum=0 degree=4 index=0 axis=1
rTraceRd :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
rTraceRd rate bufnum degree index_ axis = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "RTraceRd" [bufnum,degree,index_,axis] Nothing 1 (Special 0) NoId

-- | differential pulse-code modulation
--
--  RedDPCMdecode [ControlRate,AudioRate] in=0
redDPCMdecode :: Rate -> UGen -> UGen
redDPCMdecode rate in_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "RedDPCMdecode" [in_] Nothing 1 (Special 0) NoId

-- | differential pulse-code modulation
--
--  RedDPCMencode [ControlRate,AudioRate] in=0 round=0
redDPCMencode :: Rate -> UGen -> UGen -> UGen
redDPCMencode rate in_ round_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "RedDPCMencode" [in_,round_] Nothing 1 (Special 0) NoId

-- | look before you leap
--
--  RedLbyl [ControlRate,AudioRate] in=0 thresh=0.5 samples=2
redLbyl :: Rate -> UGen -> UGen -> UGen -> UGen
redLbyl rate in_ thresh samples = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "RedLbyl" [in_,thresh,samples] Nothing 1 (Special 0) NoId

-- | a really bad pseudo-random noise generator
--
--  RedNoise [ControlRate,AudioRate] clock=0
redNoise :: Rate -> UGen -> UGen
redNoise rate clock = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "RedNoise" [clock] Nothing 1 (Special 0) NoId

-- | a phasor that can loop
--
--  RedPhasor [ControlRate,AudioRate] trig=0 rate=1 start=0 end=1 loop=0 loopstart=0 loopend=1
redPhasor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
redPhasor rate trig_ rate_ start end loop loopstart loopend = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "RedPhasor" [trig_,rate_,start,end,loop,loopstart,loopend] Nothing 1 (Special 0) NoId

-- | a phasor that can loop - version2
--
--  RedPhasor2 [ControlRate,AudioRate] trig=0 rate=1 start=0 end=1 loop=0 loopstart=0 loopend=1
redPhasor2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
redPhasor2 rate trig_ rate_ start end loop loopstart loopend = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "RedPhasor2" [trig_,rate_,start,end,loop,loopstart,loopend] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RegaliaMitraEQ [AudioRate] in=0 freq=440 rq=0.1 k=0
regaliaMitraEQ :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
regaliaMitraEQ rate in_ freq rq k = mkUGen Nothing [AudioRate] (Left rate) "RegaliaMitraEQ" [in_,freq,rq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Resonator [AudioRate] input=0 freq=100 position=0.001 resolution=24 structure=0.5 brightness=0.5 damping=0.5;    FILTER: TRUE
resonator :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
resonator input freq position resolution structure brightness damping = mkUGen Nothing [AudioRate] (Right [0]) "Resonator" [input,freq,position,resolution,structure,brightness,damping] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Rongs [AudioRate] trigger=0 sustain=1 f0=0.01 structure=0.5 brightness=0.5 damping=0.75 accent=0.9 stretch=0.5 position=0.15 loss=0.15 modeNum=2 cosFreq=0.25
rongs :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rongs rate trigger sustain f0 structure brightness damping accent stretch position loss modeNum cosFreq = mkUGen Nothing [AudioRate] (Left rate) "Rongs" [trigger,sustain,f0,structure,brightness,damping,accent,stretch,position,loss,modeNum,cosFreq] Nothing 1 (Special 0) NoId

-- | Rossler chaotic generator
--
--  RosslerL [AudioRate] freq=22050 a=0.2 b=0.2 c=5.7 h=0.05 xi=0.1 yi=0 zi=0
rosslerL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rosslerL rate freq a b c h xi yi zi = mkUGen Nothing [AudioRate] (Left rate) "RosslerL" [freq,a,b,c,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  RosslerResL [AudioRate] in=0 stiff=1 freq=22050 a=0.2 b=0.2 c=5.7 h=0.05 xi=0.1 yi=0 zi=0
rosslerResL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rosslerResL rate in_ stiff freq a b c h xi yi zi = mkUGen Nothing [AudioRate] (Left rate) "RosslerResL" [in_,stiff,freq,a,b,c,h,xi,yi,zi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Rotate [AudioRate] w=0 x=0 y=0 z=0 rotate=0
rotate :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rotate rate w x y z rotate_ = mkUGen Nothing [AudioRate] (Left rate) "Rotate" [w,x,y,z,rotate_] Nothing 1 (Special 0) NoId

-- | (faulty) SID Sound Chip Simulator
--
--  SID6581f [AudioRate] freqLo0=0 freqHi0=0 pwLo0=0 pwHi0=0 ctrl0=0 atkDcy0=0 susRel0=0 freqLo1=0 freqHi1=0 pwLo1=0 pwHi1=0 ctrl1=0 atkDcy1=0 susRel1=0 freqLo2=0 freqHi2=0 pwLo2=0 pwHi2=0 ctrl2=0 atkDcy2=0 susRel2=0 fcLo=0 fcHi=0 resFilt=0 modeVol=0 rate=1
sid6581f :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sid6581f rate freqLo0 freqHi0 pwLo0 pwHi0 ctrl0 atkDcy0 susRel0 freqLo1 freqHi1 pwLo1 pwHi1 ctrl1 atkDcy1 susRel1 freqLo2 freqHi2 pwLo2 pwHi2 ctrl2 atkDcy2 susRel2 fcLo fcHi resFilt modeVol rate_ = mkUGen Nothing [AudioRate] (Left rate) "SID6581f" [freqLo0,freqHi0,pwLo0,pwHi0,ctrl0,atkDcy0,susRel0,freqLo1,freqHi1,pwLo1,pwHi1,ctrl1,atkDcy1,susRel1,freqLo2,freqHi2,pwLo2,pwHi2,ctrl2,atkDcy2,susRel2,fcLo,fcHi,resFilt,modeVol,rate_] Nothing 1 (Special 0) NoId

-- | experimental time domain onset detector
--
--  SLOnset [ControlRate] input=0 memorysize1=20 before=5 after=5 threshold=10 hysteresis=10
slOnset :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
slOnset rate input memorysize1 before after threshold hysteresis = mkUGen Nothing [ControlRate] (Left rate) "SLOnset" [input,memorysize1,before,after,threshold,hysteresis] Nothing 1 (Special 0) NoId

-- | Spectral Modeling Synthesis
--
--  SMS [AudioRate] input=0 maxpeaks=80 currentpeaks=80 tolerance=4 noisefloor=0.2 freqmult=1 freqadd=0 formantpreserve=0 useifft=0 ampmult=1 graphicsbufnum=0;    FILTER: TRUE
sms :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sms input maxpeaks currentpeaks tolerance noisefloor freqmult freqadd formantpreserve useifft ampmult graphicsbufnum = mkUGen Nothing [AudioRate] (Right [0]) "SMS" [input,maxpeaks,currentpeaks,tolerance,noisefloor,freqmult,freqadd,formantpreserve,useifft,ampmult,graphicsbufnum] Nothing 2 (Special 0) NoId

-- | Sound Chip Simulator
--
--  SN76489 [AudioRate] tone0=512 tone1=0 tone2=0 noise=0 vol0=15 vol1=0 vol2=0 vol3=0 rate=1
sn76489 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sn76489 rate tone0 tone1 tone2 noise vol0 vol1 vol2 vol3 rate_ = mkUGen Nothing [AudioRate] (Left rate) "SN76489" [tone0,tone1,tone2,noise,vol0,vol1,vol2,vol3,rate_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SOMAreaWr [ControlRate] bufnum=0 inputdata=0 coords=0 netsize=10 numdims=2 nhood=0.5 gate=1
somAreaWr :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
somAreaWr rate bufnum inputdata coords netsize numdims nhood gate_ = mkUGen Nothing [ControlRate] (Left rate) "SOMAreaWr" [bufnum,inputdata,coords,netsize,numdims,nhood,gate_] Nothing 1 (Special 0) NoId

-- | Map an input using a Self-Organising Map
--
--  SOMRd [ControlRate,AudioRate] bufnum=0 inputdata=0 netsize=10 numdims=2 gate=1
somRd :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
somRd rate bufnum inputdata netsize numdims gate_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "SOMRd" [bufnum,inputdata,netsize,numdims,gate_] Nothing 2 (Special 0) NoId

-- | Create (train) a Self-Organising Map
--
--  SOMTrain [ControlRate] bufnum=0 inputdata=0 netsize=10 numdims=2 traindur=5000 nhood=0.5 gate=1 initweight=1
somTrain :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
somTrain rate bufnum inputdata netsize numdims traindur nhood gate_ initweight = mkUGen Nothing [ControlRate] (Left rate) "SOMTrain" [bufnum,inputdata,netsize,numdims,traindur,nhood,gate_,initweight] Nothing 3 (Special 0) NoId

-- | 12db/Oct State Variable Filter
--
--  SVF [ControlRate,AudioRate] signal=0 cutoff=2200 res=0.1 lowpass=1 bandpass=0 highpass=0 notch=0 peak=0;    FILTER: TRUE
svf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
svf signal cutoff res lowpass bandpass highpass notch peak_ = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "SVF" [signal,cutoff,res,lowpass,bandpass,highpass,notch,peak_] Nothing 1 (Special 0) NoId

-- | super-efficient sawtooth oscillator with low aliasing
--
--  SawDPW [ControlRate,AudioRate] freq=440 iphase=0
sawDPW :: Rate -> UGen -> UGen -> UGen
sawDPW rate freq iphase = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "SawDPW" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Perceptual feature modeling sensory dissonance
--
--  SensoryDissonance [ControlRate] fft=0 maxpeaks=100 peakthreshold=0.1 norm=0 clamp=1
sensoryDissonance :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sensoryDissonance rate fft_ maxpeaks peakthreshold norm clamp = mkUGen Nothing [ControlRate] (Left rate) "SensoryDissonance" [fft_,maxpeaks,peakthreshold,norm,clamp] Nothing 1 (Special 0) NoId

-- | Fuzzy sieve based synthesis
--
--  Sieve1 [ControlRate,AudioRate] bufnum=0 gap=2 alternate=1
sieve1 :: Rate -> UGen -> UGen -> UGen -> UGen
sieve1 rate bufnum gap alternate = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Sieve1" [bufnum,gap,alternate] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains
--
--  SinGrain [AudioRate] trigger=0 dur=1 freq=440
sinGrain :: Rate -> UGen -> UGen -> UGen -> UGen
sinGrain rate trigger dur freq = mkUGen Nothing [AudioRate] (Left rate) "SinGrain" [trigger,dur,freq] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains and user supplied envelope
--
--  SinGrainB [AudioRate] trigger=0 dur=1 freq=440 envbuf=0
sinGrainB :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
sinGrainB rate trigger dur freq envbuf = mkUGen Nothing [AudioRate] (Left rate) "SinGrainB" [trigger,dur,freq,envbuf] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains with Ambisonic panning and user supplied envelope
--
--  SinGrainBBF [AudioRate] trigger=0 dur=1 freq=440 envbuf=0 azimuth=0 elevation=0 rho=1 wComp=0
sinGrainBBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sinGrainBBF rate trigger dur freq envbuf azimuth elevation rho wComp = mkUGen Nothing [AudioRate] (Left rate) "SinGrainBBF" [trigger,dur,freq,envbuf,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains with Ambisonic panning
--
--  SinGrainBF [AudioRate] trigger=0 dur=1 freq=440 azimuth=0 elevation=0 rho=1 wComp=0
sinGrainBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sinGrainBF rate trigger dur freq azimuth elevation rho wComp = mkUGen Nothing [AudioRate] (Left rate) "SinGrainBF" [trigger,dur,freq,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains and user supplied envelopes
--
--  SinGrainI [AudioRate] trigger=0 dur=1 freq=440 envbuf1=0 envbuf2=0 ifac=0.5
sinGrainI :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sinGrainI rate trigger dur freq envbuf1 envbuf2 ifac = mkUGen Nothing [AudioRate] (Left rate) "SinGrainI" [trigger,dur,freq,envbuf1,envbuf2,ifac] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains with Ambisonic panning and user supplied envelopes
--
--  SinGrainIBF [AudioRate] trigger=0 dur=1 freq=440 envbuf1=0 envbuf2=0 ifac=0.5 azimuth=0 elevation=0 rho=1 wComp=0
sinGrainIBF :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sinGrainIBF rate trigger dur freq envbuf1 envbuf2 ifac azimuth elevation rho wComp = mkUGen Nothing [AudioRate] (Left rate) "SinGrainIBF" [trigger,dur,freq,envbuf1,envbuf2,ifac,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  SinTone [AudioRate] freq=440 phase=0
sinTone :: Rate -> UGen -> UGen -> UGen
sinTone rate freq phase = mkUGen Nothing [AudioRate] (Left rate) "SinTone" [freq,phase] Nothing 1 (Special 0) NoId

-- | port of some ladspa plugins
--
--  SineShaper [AudioRate] in=0 limit=1;    FILTER: TRUE
sineShaper :: UGen -> UGen -> UGen
sineShaper in_ limit = mkUGen Nothing [AudioRate] (Right [0]) "SineShaper" [in_,limit] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SkipNeedle [AudioRate] range=44100 rate=10 offset=0
skipNeedle :: Rate -> UGen -> UGen -> UGen -> UGen
skipNeedle rate range rate_ offset = mkUGen Nothing [AudioRate] (Left rate) "SkipNeedle" [range,rate_,offset] Nothing 1 (Special 0) NoId

-- | lowpass filter for envelope following
--
--  Slide [ControlRate,AudioRate] in=0 slideup=50 slidedown=3000
slide :: Rate -> UGen -> UGen -> UGen -> UGen
slide rate in_ slideup slidedown = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Slide" [in_,slideup,slidedown] Nothing 1 (Special 0) NoId

-- | generate cpu spikes
--
--  Slub [ControlRate,AudioRate] trig=0 spike=4.04
slub :: Rate -> UGen -> UGen -> UGen
slub rate trig_ spike = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Slub" [trig_,spike] Nothing 1 (Special 0) NoId

-- | Smooth samplerate and bitrate reduction
--
--  SmoothDecimator [AudioRate] in=0 rate=44100 smoothing=0.5
smoothDecimator :: Rate -> UGen -> UGen -> UGen -> UGen
smoothDecimator rate in_ rate_ smoothing = mkUGen Nothing [AudioRate] (Left rate) "SmoothDecimator" [in_,rate_,smoothing] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipAmp [AudioRate] in=0 pregain=1;    FILTER: TRUE
softClipAmp :: UGen -> UGen -> UGen
softClipAmp in_ pregain = mkUGen Nothing [AudioRate] (Right [0]) "SoftClipAmp" [in_,pregain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipAmp4 [AudioRate] in=0 pregain=1;    FILTER: TRUE
softClipAmp4 :: UGen -> UGen -> UGen
softClipAmp4 in_ pregain = mkUGen Nothing [AudioRate] (Right [0]) "SoftClipAmp4" [in_,pregain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipAmp8 [AudioRate] in=0 pregain=1;    FILTER: TRUE
softClipAmp8 :: UGen -> UGen -> UGen
softClipAmp8 in_ pregain = mkUGen Nothing [AudioRate] (Right [0]) "SoftClipAmp8" [in_,pregain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipper4 [AudioRate] in=0
softClipper4 :: Rate -> UGen -> UGen
softClipper4 rate in_ = mkUGen Nothing [AudioRate] (Left rate) "SoftClipper4" [in_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipper8 [AudioRate] in=0
softClipper8 :: Rate -> UGen -> UGen
softClipper8 rate in_ = mkUGen Nothing [AudioRate] (Left rate) "SoftClipper8" [in_] Nothing 1 (Special 0) NoId

-- | LPC analizer.
--
--  SonLPC [AudioRate] buff=-1.0 in=0.0 hop=0.5 poles=10.0
sonLPC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
sonLPC rate buff in_ hop poles = mkUGen Nothing [AudioRate] (Left rate) "SonLPC" [buff,in_,hop,poles] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SonLPCSynth [AudioRate] chain=-1.0
sonLPCSynth :: Rate -> UGen -> UGen
sonLPCSynth rate chain = mkUGen Nothing [AudioRate] (Left rate) "SonLPCSynth" [chain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SonLPCSynthIn [AudioRate] chain=-1.0 in=0.0
sonLPCSynthIn :: Rate -> UGen -> UGen -> UGen
sonLPCSynthIn rate chain in_ = mkUGen Nothing [AudioRate] (Left rate) "SonLPCSynthIn" [chain,in_] Nothing 1 (Special 0) NoId

-- | Karplus-Strong via a sorting algorithm
--
--  SortBuf [AudioRate] bufnum=0 sortrate=10 reset=0
sortBuf :: Rate -> UGen -> UGen -> UGen -> UGen
sortBuf rate bufnum sortrate reset = mkUGen Nothing [AudioRate] (Left rate) "SortBuf" [bufnum,sortrate,reset] Nothing 1 (Special 0) NoId

-- | Spectral feature extraction
--
--  SpectralEntropy [ControlRate] fft=0 fftsize=2048 numbands=1;    NC INPUT: True
spectralEntropy :: Int -> Rate -> UGen -> UGen -> UGen -> UGen
spectralEntropy numChannels rate fft_ fftsize numbands = mkUGen Nothing [ControlRate] (Left rate) "SpectralEntropy" [fft_,fftsize,numbands] Nothing numChannels (Special 0) NoId

-- | (Undocumented class)
--
--  Spreader [AudioRate] in=0 theta=1.5708 filtsPerOctave=8
spreader :: Rate -> UGen -> UGen -> UGen -> UGen
spreader rate in_ theta filtsPerOctave = mkUGen Nothing [AudioRate] (Left rate) "Spreader" [in_,theta,filtsPerOctave] Nothing 2 (Special 0) NoId

-- | Spruce bud worm model equations
--
--  SpruceBudworm [AudioRate] reset=0 rate=0.1 k1=27.9 k2=1.5 alpha=0.1 beta=10.1 mu=0.3 rho=10.1 initx=0.9 inity=0.1
spruceBudworm :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
spruceBudworm rate reset rate_ k1 k2 alpha beta mu rho initx inity = mkUGen Nothing [AudioRate] (Left rate) "SpruceBudworm" [reset,rate_,k1,k2,alpha,beta,mu,rho,initx,inity] Nothing 2 (Special 0) NoId

-- | Wave squeezer. Maybe a kind of pitch shifter.
--
--  Squiz [ControlRate,AudioRate] in=0 pitchratio=2 zcperchunk=1 memlen=0.1;    FILTER: TRUE
squiz :: UGen -> UGen -> UGen -> UGen -> UGen
squiz in_ pitchratio zcperchunk memlen = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Squiz" [in_,pitchratio,zcperchunk,memlen] Nothing 1 (Special 0) NoId

-- | standard map 2D chaotic generator
--
--  Standard2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 k=1.4 x0=4.97898 y0=5.74734
standard2DC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
standard2DC rate minfreq maxfreq k x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Standard2DC" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | standard map 2D chaotic generator
--
--  Standard2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 k=1.4 x0=4.97898 y0=5.74734
standard2DL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
standard2DL rate minfreq maxfreq k x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Standard2DL" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | standard map 2D chaotic generator
--
--  Standard2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 k=1.4 x0=4.97898 y0=5.74734
standard2DN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
standard2DN rate minfreq maxfreq k x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "Standard2DN" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StandardTrig [ControlRate,AudioRate] minfreq=5 maxfreq=10 k=1.4 x0=4.97898 y0=5.74734
standardTrig :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
standardTrig rate minfreq maxfreq k x0 y0 = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StandardTrig" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBandedWG [ControlRate,AudioRate] freq=440 instr=0 bowpressure=0 bowmotion=0 integration=0 modalresonance=64 bowvelocity=0 setstriking=0 trig=1
stkBandedWG :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkBandedWG rate freq instr bowpressure bowmotion integration modalresonance bowvelocity setstriking trig_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkBandedWG" [freq,instr,bowpressure,bowmotion,integration,modalresonance,bowvelocity,setstriking,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBeeThree [ControlRate,AudioRate] freq=440 op4gain=10 op3gain=20 lfospeed=64 lfodepth=0 adsrtarget=64 trig=1
stkBeeThree :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkBeeThree rate freq op4gain op3gain lfospeed lfodepth adsrtarget trig_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkBeeThree" [freq,op4gain,op3gain,lfospeed,lfodepth,adsrtarget,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBlowHole [ControlRate,AudioRate] freq=440 reedstiffness=64 noisegain=20 tonehole=64 register=11 breathpressure=64
stkBlowHole :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkBlowHole rate freq reedstiffness noisegain tonehole register breathpressure = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkBlowHole" [freq,reedstiffness,noisegain,tonehole,register,breathpressure] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBowed [ControlRate,AudioRate] freq=220 bowpressure=64 bowposition=64 vibfreq=64 vibgain=64 loudness=64 gate=1 attackrate=1 decayrate=1
stkBowed :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkBowed rate freq bowpressure bowposition vibfreq vibgain loudness_ gate_ attackrate decayrate = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkBowed" [freq,bowpressure,bowposition,vibfreq,vibgain,loudness_,gate_,attackrate,decayrate] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkClarinet [ControlRate,AudioRate] freq=440 reedstiffness=64 noisegain=4 vibfreq=64 vibgain=11 breathpressure=64 trig=1
stkClarinet :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkClarinet rate freq reedstiffness noisegain vibfreq vibgain breathpressure trig_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkClarinet" [freq,reedstiffness,noisegain,vibfreq,vibgain,breathpressure,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkFlute [ControlRate,AudioRate] freq=440 jetDelay=49 noisegain=0.15 jetRatio=0.32
stkFlute :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
stkFlute rate freq jetDelay noisegain jetRatio = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkFlute" [freq,jetDelay,noisegain,jetRatio] Nothing 1 (Special 0) NoId

-- | Wrapping Synthesis toolkit.
--
--  StkGlobals [AudioRate] showWarnings=0 printErrors=0 rawfilepath=0
stkGlobals :: Rate -> UGen -> UGen -> UGen -> UGen
stkGlobals rate showWarnings printErrors rawfilepath = mkUGen Nothing [AudioRate] (Left rate) "StkGlobals" [showWarnings,printErrors,rawfilepath] Nothing 1 (Special 0) NoId

-- | Wrapping Synthesis toolkit.
--
--  StkInst [AudioRate] freq=220 gate=1 onamp=1 offamp=0.5 instNumber=6 *args=0;    MCE=1, REORDERS INPUTS: [4,0,1,2,3,5]
stkInst :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkInst rate freq gate_ onamp offamp instNumber args = mkUGen Nothing [AudioRate] (Left rate) "StkInst" [freq,gate_,onamp,offamp,instNumber] (Just [args]) 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkMandolin [ControlRate,AudioRate] freq=520 bodysize=64 pickposition=64 stringdamping=69 stringdetune=10 aftertouch=64 trig=1
stkMandolin :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkMandolin rate freq bodysize pickposition stringdamping stringdetune aftertouch trig_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkMandolin" [freq,bodysize,pickposition,stringdamping,stringdetune,aftertouch,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkModalBar [ControlRate,AudioRate] freq=440 instrument=0 stickhardness=64 stickposition=64 vibratogain=20 vibratofreq=20 directstickmix=64 volume=64 trig=1
stkModalBar :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkModalBar rate freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume trig_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkModalBar" [freq,instrument,stickhardness,stickposition,vibratogain,vibratofreq,directstickmix,volume,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkMoog [ControlRate,AudioRate] freq=440 filterQ=10 sweeprate=20 vibfreq=64 vibgain=0 gain=64 trig=1
stkMoog :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkMoog rate freq filterQ sweeprate vibfreq vibgain gain trig_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkMoog" [freq,filterQ,sweeprate,vibfreq,vibgain,gain,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkPluck [ControlRate,AudioRate] freq=440 decay=0.99
stkPluck :: Rate -> UGen -> UGen -> UGen
stkPluck rate freq decay_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkPluck" [freq,decay_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkSaxofony [ControlRate,AudioRate] freq=220 reedstiffness=64 reedaperture=64 noisegain=20 blowposition=26 vibratofrequency=20 vibratogain=20 breathpressure=128 trig=1
stkSaxofony :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkSaxofony rate freq reedstiffness reedaperture noisegain blowposition vibratofrequency vibratogain breathpressure trig_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkSaxofony" [freq,reedstiffness,reedaperture,noisegain,blowposition,vibratofrequency,vibratogain,breathpressure,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkShakers [ControlRate,AudioRate] instr=0 energy=64 decay=64 objects=64 resfreq=64
stkShakers :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkShakers rate instr energy decay_ objects resfreq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkShakers" [instr,energy,decay_,objects,resfreq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkVoicForm [ControlRate,AudioRate] freq=440 vuvmix=64 vowelphon=64 vibfreq=64 vibgain=20 loudness=64 trig=1
stkVoicForm :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkVoicForm rate freq vuvmix vowelphon vibfreq vibgain loudness_ trig_ = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "StkVoicForm" [freq,vuvmix,vowelphon,vibfreq,vibgain,loudness_,trig_] Nothing 1 (Special 0) NoId

-- | String resonance filter
--
--  Streson [ControlRate,AudioRate] input=0 delayTime=0.003 res=0.9;    FILTER: TRUE
streson :: UGen -> UGen -> UGen -> UGen
streson input delayTime res = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Streson" [input,delayTime,res] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StringVoice [AudioRate] trig=0 infsustain=0 freq=100 accent=0.5 structure=0.5 brightness=0.5 damping=0.5
stringVoice :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stringVoice rate trig_ infsustain freq accent structure brightness damping = mkUGen Nothing [AudioRate] (Left rate) "StringVoice" [trig_,infsustain,freq,accent,structure,brightness,damping] Nothing 1 (Special 0) NoId

-- | Pulse counter with floating point steps
--
--  Summer [ControlRate,AudioRate] trig=0 step=1 reset=0 resetval=0;    FILTER: TRUE
summer :: UGen -> UGen -> UGen -> UGen -> UGen
summer trig_ step reset resetval = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "Summer" [trig_,step,reset,resetval] Nothing 1 (Special 0) NoId

-- | feedback delay line implementing switch-and-ramp buffer jumping
--
--  SwitchDelay [AudioRate] in=0 drylevel=1 wetlevel=1 delaytime=1 delayfactor=0.7 maxdelaytime=20;    FILTER: TRUE
switchDelay :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
switchDelay in_ drylevel wetlevel delaytime delayfactor maxdelaytime = mkUGen Nothing [AudioRate] (Right [0]) "SwitchDelay" [in_,drylevel,wetlevel,delaytime,delayfactor,maxdelaytime] Nothing 1 (Special 0) NoId

-- | triggered beta random distribution
--
--  TBetaRand [ControlRate,AudioRate] lo=0 hi=1 prob1=0 prob2=0 trig=0;    FILTER: TRUE, NONDET
tBetaRandId :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tBetaRandId z lo hi prob1 prob2 trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [4]) "TBetaRand" [lo,hi,prob1,prob2,trig_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of TBetaRand.
tBetaRandM :: UId m => UGen -> UGen -> UGen -> UGen -> UGen -> m UGen
tBetaRandM = liftUId5 tBetaRandId

-- | Unsafe variant of TBetaRand.
tBetaRand ::  UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tBetaRand = liftUnsafe5 tBetaRandM

-- | triggered random walk generator
--
--  TBrownRand [ControlRate,AudioRate] lo=0 hi=1 dev=1 dist=0 trig=0;    FILTER: TRUE, NONDET
tBrownRandId :: ID a => a -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tBrownRandId z lo hi dev dist trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [4]) "TBrownRand" [lo,hi,dev,dist,trig_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of TBrownRand.
tBrownRandM :: UId m => UGen -> UGen -> UGen -> UGen -> UGen -> m UGen
tBrownRandM = liftUId5 tBrownRandId

-- | Unsafe variant of TBrownRand.
tBrownRand ::  UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tBrownRand = liftUnsafe5 tBrownRandM

-- | triggered gaussian random distribution
--
--  TGaussRand [ControlRate,AudioRate] lo=0 hi=1 trig=0;    FILTER: TRUE, NONDET
tGaussRandId :: ID a => a -> UGen -> UGen -> UGen -> UGen
tGaussRandId z lo hi trig_ = mkUGen Nothing [ControlRate,AudioRate] (Right [2]) "TGaussRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUId z)

-- | Monad variant of TGaussRand.
tGaussRandM :: UId m => UGen -> UGen -> UGen -> m UGen
tGaussRandM = liftUId3 tGaussRandId

-- | Unsafe variant of TGaussRand.
tGaussRand ::  UGen -> UGen -> UGen -> UGen
tGaussRand = liftUnsafe3 tGaussRandM

-- | buffer granulator with linear att/dec
--
--  TGrains2 [AudioRate] trigger=0 bufnum=0 rate=1 centerPos=0 dur=0.1 pan=0 amp=0.1 att=0.5 dec=0.5 interp=4;    NC INPUT: True
tGrains2 :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains2 numChannels rate trigger bufnum rate_ centerPos dur pan amp att dec interp = mkUGen Nothing [AudioRate] (Left rate) "TGrains2" [trigger,bufnum,rate_,centerPos,dur,pan,amp,att,dec,interp] Nothing numChannels (Special 0) NoId

-- | buffer granulator with user envelope
--
--  TGrains3 [AudioRate] trigger=0 bufnum=0 rate=1 centerPos=0 dur=0.1 pan=0 amp=0.1 att=0.5 dec=0.5 window=1 interp=4;    NC INPUT: True
tGrains3 :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains3 numChannels rate trigger bufnum rate_ centerPos dur pan amp att dec window interp = mkUGen Nothing [AudioRate] (Left rate) "TGrains3" [trigger,bufnum,rate_,centerPos,dur,pan,amp,att,dec,window,interp] Nothing numChannels (Special 0) NoId

-- | Tracking Phase Vocoder
--
--  TPV [AudioRate] chain=0 windowsize=1024 hopsize=512 maxpeaks=80 currentpeaks=0 freqmult=1 tolerance=4 noisefloor=0.2
tpv :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tpv chain windowsize hopsize maxpeaks currentpeaks freqmult tolerance noisefloor = mkUGen Nothing [AudioRate] (Left AudioRate) "TPV" [chain,windowsize,hopsize,maxpeaks,currentpeaks,freqmult,tolerance,noisefloor] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  TTendency [ControlRate,AudioRate] trigger=0 dist=0 parX=0 parY=1 parA=0 parB=0
tTendency :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tTendency rate trigger dist parX parY parA parB = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "TTendency" [trigger,dist,parX,parY,parA,parB] Nothing 1 (Special 0) NoId

-- | pitch tracker
--
--  Tartini [ControlRate] in=0 threshold=0.93 n=2048 k=0 overlap=1024 smallCutoff=0.5
tartini :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tartini rate in_ threshold n k overlap smallCutoff = mkUGen Nothing [ControlRate] (Left rate) "Tartini" [in_,threshold,n,k,overlap,smallCutoff] Nothing 2 (Special 0) NoId

-- | Neural Oscillator
--
--  TermanWang [AudioRate] input=0 reset=0 ratex=0.01 ratey=0.01 alpha=1 beta=1 eta=1 initx=0 inity=0
termanWang :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
termanWang rate input reset ratex ratey alpha beta eta initx inity = mkUGen Nothing [AudioRate] (Left rate) "TermanWang" [input,reset,ratex,ratey,alpha,beta,eta,initx,inity] Nothing 1 (Special 0) NoId

-- | display level of a UGen as a textual meter
--
--  TextVU [ControlRate,AudioRate] trig=2 in=0 label=0 width=21 reset=0 ana=0
textVU ::  Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
textVU rate trig_ in_ label_ width reset ana = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "TextVU" [trig_,in_,label_,width,reset,ana] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Tilt [AudioRate] w=0 x=0 y=0 z=0 tilt=0
tilt :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tilt rate w x y z tilt_ = mkUGen Nothing [AudioRate] (Left rate) "Tilt" [w,x,y,z,tilt_] Nothing 1 (Special 0) NoId

-- | triggered signal averager
--
--  TrigAvg [ControlRate] in=0 trig=0
trigAvg :: Rate -> UGen -> UGen -> UGen
trigAvg rate in_ trig_ = mkUGen Nothing [ControlRate] (Left rate) "TrigAvg" [in_,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Tumble [AudioRate] w=0 x=0 y=0 z=0 tilt=0
tumble :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tumble rate w x y z tilt_ = mkUGen Nothing [AudioRate] (Left rate) "Tumble" [w,x,y,z,tilt_] Nothing 1 (Special 0) NoId

-- | physical modeling simulation; two tubes
--
--  TwoTube [AudioRate] input=0 k=0.01 loss=1 d1length=100 d2length=100
twoTube :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
twoTube rate input k loss d1length d2length = mkUGen Nothing [AudioRate] (Left rate) "TwoTube" [input,k,loss,d1length,d2length] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  UHJ2B [AudioRate] ls=0 rs=0
uhj2b :: Rate -> UGen -> UGen -> UGen
uhj2b rate ls rs = mkUGen Nothing [AudioRate] (Left rate) "UHJ2B" [ls,rs] Nothing 3 (Special 0) NoId

-- | Vector Base Amplitude Panner
--
--  VBAP [ControlRate,AudioRate] in=0 bufnum=0 azimuth=0 elevation=1 spread=0;    NC INPUT: True
vbap :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
vbap numChannels rate in_ bufnum azimuth elevation spread = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "VBAP" [in_,bufnum,azimuth,elevation,spread] Nothing numChannels (Special 0) NoId

-- | a Chebyshev low/highpass filter
--
--  VBChebyFilt [AudioRate] in=0 freq=880 mode=0 order=4
vbChebyFilt :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vbChebyFilt rate in_ freq mode order = mkUGen Nothing [AudioRate] (Left rate) "VBChebyFilt" [in_,freq,mode,order] Nothing 1 (Special 0) NoId

-- | a chaotic oscillator network
--
--  VBFourses [AudioRate] smoother=0.5 *freqarray=0;    MCE=1, REORDERS INPUTS: [1,0]
vbFourses :: Rate -> UGen -> UGen -> UGen
vbFourses rate smoother freqarray = mkUGen Nothing [AudioRate] (Left rate) "VBFourses" [smoother] (Just [freqarray]) 4 (Special 0) NoId

-- | artifical reverberator
--
--  VBJonVerb [AudioRate] in=0 decay=0.6 damping=0.3 inputbw=0.8 erfl=0.5 tail=0.5;    FILTER: TRUE
vbJonVerb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
vbJonVerb in_ decay_ damping inputbw erfl tail_ = mkUGen Nothing [AudioRate] (Right [0]) "VBJonVerb" [in_,decay_,damping,inputbw,erfl,tail_] Nothing 2 (Special 0) NoId

-- | a simple phase vocoder for time-stretching
--
--  VBPVoc [AudioRate] numChannels=0 bufnum=0 playpos=0 fftsize=2048
vbpVoc :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vbpVoc rate numChannels bufnum playpos fftsize = mkUGen Nothing [AudioRate] (Left rate) "VBPVoc" [numChannels,bufnum,playpos,fftsize] Nothing 1 (Special 0) NoId

-- | lowpass filter for envelope following
--
--  VBSlide [KR,AR] in=0.0 slideup=50.0 slidedown=3000.0;    FILTER: TRUE
vbSlide :: UGen -> UGen -> UGen -> UGen
vbSlide in_ slideup slidedown = mkUGen Nothing [ControlRate,AudioRate] (Right [0]) "VBSlide" [in_,slideup,slidedown] Nothing 1 (Special 0) NoId

-- | 2D scanning pattern virtual machine
--
--  VMScan2D [AudioRate] bufnum=0
vmScan2D :: Rate -> UGen -> UGen
vmScan2D rate bufnum = mkUGen Nothing [AudioRate] (Left rate) "VMScan2D" [bufnum] Nothing 2 (Special 0) NoId

-- | vosim pulse generator
--
--  VOSIM [AudioRate] trig=0.1 freq=400 nCycles=1 decay=0.9
vosim :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vosim rate trig_ freq nCycles decay_ = mkUGen Nothing [AudioRate] (Left rate) "VOSIM" [trig_,freq,nCycles,decay_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  VarShapeOsc [ControlRate,AudioRate] freq=100 pw=0.5 waveshape=0.5 sync=1 syncfreq=105
varShapeOsc :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
varShapeOsc rate freq pw waveshape sync syncfreq = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "VarShapeOsc" [freq,pw,waveshape,sync,syncfreq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  VosimOsc [ControlRate,AudioRate] freq=100 form1freq=951 form2freq=919 shape=0
vosimOsc :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vosimOsc rate freq form1freq form2freq shape = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "VosimOsc" [freq,form1freq,form2freq,shape] Nothing 1 (Special 0) NoId

-- | windowed amplitude follower
--
--  WAmp [ControlRate] in=0 winSize=0.1
wAmp :: Rate -> UGen -> UGen -> UGen
wAmp rate in_ winSize = mkUGen Nothing [ControlRate] (Left rate) "WAmp" [in_,winSize] Nothing 1 (Special 0) NoId

-- | decomposition into square waves, and reconstruction
--
--  WalshHadamard [AudioRate] input=0 which=0
walshHadamard :: Rate -> UGen -> UGen -> UGen
walshHadamard rate input which = mkUGen Nothing [AudioRate] (Left rate) "WalshHadamard" [input,which] Nothing 1 (Special 0) NoId

-- | Warp a buffer with a time pointer
--
--  WarpZ [AudioRate] bufnum=0 pointer=0 freqScale=1 windowSize=0.2 envbufnum=-1 overlaps=8 windowRandRatio=0 interp=1 zeroSearch=0 zeroStart=0;    NC INPUT: True
warpZ :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
warpZ numChannels rate bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp zeroSearch zeroStart = mkUGen Nothing [AudioRate] (Left rate) "WarpZ" [bufnum,pointer,freqScale,windowSize,envbufnum,overlaps,windowRandRatio,interp,zeroSearch,zeroStart] Nothing numChannels (Special 0) NoId

-- | Lose bits of your waves
--
--  WaveLoss [ControlRate,AudioRate] in=0 drop=20 outof=40 mode=1
waveLoss :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
waveLoss rate in_ drop_ outof mode = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "WaveLoss" [in_,drop_,outof,mode] Nothing 1 (Special 0) NoId

-- | wave terrain synthesis
--
--  WaveTerrain [AudioRate] bufnum=0 x=0 y=0 xsize=100 ysize=100
waveTerrain :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
waveTerrain rate bufnum x y xsize ysize = mkUGen Nothing [AudioRate] (Left rate) "WaveTerrain" [bufnum,x,y,xsize,ysize] Nothing 1 (Special 0) NoId

-- | decomposition into Daub4 wavelets, and reconstruction
--
--  WaveletDaub [AudioRate] input=0 n=64 which=0
waveletDaub :: Rate -> UGen -> UGen -> UGen -> UGen
waveletDaub rate input n which = mkUGen Nothing [AudioRate] (Left rate) "WaveletDaub" [input,n,which] Nothing 1 (Special 0) NoId

-- | Weakly Nonlinear Oscillator
--
--  WeaklyNonlinear [AudioRate] input=0 reset=0 ratex=1 ratey=1 freq=440 initx=0 inity=0 alpha=0 xexponent=0 beta=0 yexponent=0
weaklyNonlinear :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
weaklyNonlinear rate input reset ratex ratey freq initx inity alpha xexponent beta yexponent = mkUGen Nothing [AudioRate] (Left rate) "WeaklyNonlinear" [input,reset,ratex,ratey,freq,initx,inity,alpha,xexponent,beta,yexponent] Nothing 1 (Special 0) NoId

-- | Weakly Nonlinear Oscillator
--
--  WeaklyNonlinear2 [AudioRate] input=0 reset=0 ratex=1 ratey=1 freq=440 initx=0 inity=0 alpha=0 xexponent=0 beta=0 yexponent=0
weaklyNonlinear2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
weaklyNonlinear2 rate input reset ratex ratey freq initx inity alpha xexponent beta yexponent = mkUGen Nothing [AudioRate] (Left rate) "WeaklyNonlinear2" [input,reset,ratex,ratey,freq,initx,inity,alpha,xexponent,beta,yexponent] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Werner [AudioRate] input=0 freq=0.5 damp=0.5 feedback=0.5 drive=0 oversample=1
werner :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
werner rate input freq damp feedback drive oversample = mkUGen Nothing [AudioRate] (Left rate) "Werner" [input,freq,damp,feedback,drive,oversample] Nothing 1 (Special 0) NoId

-- | Pulse counter with floating point steps
--
--  WrapSummer [ControlRate,AudioRate] trig=0 step=1 min=0 max=1 reset=0 resetval=0
wrapSummer :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
wrapSummer rate trig_ step min_ max_ reset resetval = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "WrapSummer" [trig_,step,min_,max_,reset,resetval] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  ZOsc [ControlRate,AudioRate] freq=100 formantfreq=91 shape=0.5 mode=0.5
zOsc :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
zOsc rate freq formantfreq shape mode = mkUGen Nothing [ControlRate,AudioRate] (Left rate) "ZOsc" [freq,formantfreq,shape,mode] Nothing 1 (Special 0) NoId
