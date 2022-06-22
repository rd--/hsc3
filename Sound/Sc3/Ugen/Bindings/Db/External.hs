-- | SC3 Ugen bindings (auto-generated).
module Sound.Sc3.Ugen.Bindings.Db.External where

--import Sound.Sc3.Common.Enum
--import Sound.Sc3.Common.Envelope
import Sound.Sc3.Common.Rate
import Sound.Sc3.Common.Uid
import Sound.Sc3.Common.Unsafe

import Sound.Sc3.Ugen.Primitive
import Sound.Sc3.Ugen.Type
import Sound.Sc3.Ugen.Ugen

-- | (Undocumented class)
--
--  A2B [AudioRate] a=0 b=0 c=0 d=0
a2b :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
a2b rate a b c d = mkUgen Nothing [AudioRate] (Left rate) "A2B" [a,b,c,d] Nothing 4 (Special 0) NoId

-- | Emulator of the AY (aka YM) soundchip, used in Spectrum/Atari
--
--  AY [AudioRate] tonea=1777 toneb=1666 tonec=1555 noise=1 control=7 vola=15 volb=15 volc=15 envfreq=4 envstyle=1 chiptype=0
ay :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
ay rate tonea toneb tonec noise control_ vola volb volc envfreq envstyle chiptype = mkUgen Nothing [AudioRate] (Left rate) "AY" [tonea,toneb,tonec,noise,control_,vola,volb,volc,envfreq,envstyle,chiptype] Nothing 1 (Special 0) NoId

-- | AY-3-891X Chip Sound Simulator
--
--  AY8910 [AudioRate] r0=0 r1=0 r2=0 r3=0 r4=0 r5=0 r6=0 r7=0 r8=0 r9=0 rA=0 rB=0 rC=0 rD=0 rate=1
ay8910 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
ay8910 rate r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 rA rB rC rD rate_ = mkUgen Nothing [AudioRate] (Left rate) "AY8910" [r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,rA,rB,rC,rD,rate_] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  Allpass1 [AudioRate] in=0 freq=1200
allpass1 :: Rate -> Ugen -> Ugen -> Ugen
allpass1 rate in_ freq = mkUgen Nothing [AudioRate] (Left rate) "Allpass1" [in_,freq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Allpass2 [AudioRate] in=0 freq=1200 rq=1
allpass2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
allpass2 rate in_ freq rq = mkUgen Nothing [AudioRate] (Left rate) "Allpass2" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | amplitude follower (deprecated)
--
--  AmplitudeMod [ControlRate,AudioRate] in=0 attackTime=0.01 releaseTime=0.01
amplitudeMod :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
amplitudeMod rate in_ attackTime releaseTime = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "AmplitudeMod" [in_,attackTime,releaseTime] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogBassDrum [AudioRate] trig=0 infsustain=0 accent=0.5 freq=50 tone=0.5 decay=0.5 attackfm=0.5 selffm=0.25
analogBassDrum :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
analogBassDrum rate trig_ infsustain accent freq tone decay_ attackfm selffm = mkUgen Nothing [AudioRate] (Left rate) "AnalogBassDrum" [trig_,infsustain,accent,freq,tone,decay_,attackfm,selffm] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogFoldOsc [AudioRate] freq=100 amp=1
analogFoldOsc :: Rate -> Ugen -> Ugen -> Ugen
analogFoldOsc rate freq amp = mkUgen Nothing [AudioRate] (Left rate) "AnalogFoldOsc" [freq,amp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogPhaser [AudioRate] input=0 lfoinput=0 skew=0 feedback=0.25 modulation=0.5 stages=8;    FILTER: TRUE
analogPhaser :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
analogPhaser input lfoinput skew feedback modulation stages = mkUgen Nothing [AudioRate] (Right [0]) "AnalogPhaser" [input,lfoinput,skew,feedback,modulation,stages] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogPhaserMod [ControlRate,AudioRate] input=0 skew=0 modulation=0.5 stages=8;    FILTER: TRUE
analogPhaserMod :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
analogPhaserMod input skew modulation stages = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "AnalogPhaserMod" [input,skew,modulation,stages] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogPulseShaper [AudioRate] pulseinput=0 width=0.5 decay=0.5 double=0.5
analogPulseShaper :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
analogPulseShaper rate pulseinput width decay_ double = mkUgen Nothing [AudioRate] (Left rate) "AnalogPulseShaper" [pulseinput,width,decay_,double] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogSnareDrum [AudioRate] trig=0 infsustain=0 accent=0.1 freq=200 tone=0.5 decay=0.5 snappy=0.5
analogSnareDrum :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
analogSnareDrum rate trig_ infsustain accent freq tone decay_ snappy = mkUgen Nothing [AudioRate] (Left rate) "AnalogSnareDrum" [trig_,infsustain,accent,freq,tone,decay_,snappy] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogTape [AudioRate] input=0 bias=0.5 saturation=0.5 drive=0.5 oversample=1 mode=0;    FILTER: TRUE
analogTape :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
analogTape input bias saturation drive oversample mode = mkUgen Nothing [AudioRate] (Right [0]) "AnalogTape" [input,bias,saturation,drive,oversample,mode] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AnalogVintageDistortion [AudioRate] input=0 drivegain=0.5 bias=0 lowgain=0.1 highgain=0.1 shelvingfreq=600 oversample=0;    FILTER: TRUE
analogVintageDistortion :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
analogVintageDistortion input drivegain bias lowgain highgain shelvingfreq oversample = mkUgen Nothing [AudioRate] (Right [0]) "AnalogVintageDistortion" [input,drivegain,bias,lowgain,highgain,shelvingfreq,oversample] Nothing 1 (Special 0) NoId

-- | event analyser (BBCut)
--
--  AnalyseEvents2 [AudioRate] in=0 bufnum=0 threshold=0.34 triggerid=101 circular=0 pitch=0
analyseEvents2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
analyseEvents2 rate in_ bufnum threshold triggerid circular pitch_ = mkUgen Nothing [AudioRate] (Left rate) "AnalyseEvents2" [in_,bufnum,threshold,triggerid,circular,pitch_] Nothing 1 (Special 0) NoId

-- | 2-species Predator-Prey model
--
--  ArneodoCoulletTresser [AudioRate] freq=22050 alpha=1.5 h=0.05 xi=0.5 yi=0.5 zi=0.5
arneodoCoulletTresser :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
arneodoCoulletTresser rate freq alpha h xi yi zi = mkUgen Nothing [AudioRate] (Left rate) "ArneodoCoulletTresser" [freq,alpha,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | detect the largest value (and its position) in an array of Ugens
--
--  ArrayMax [ControlRate,AudioRate] *array=0;    MCE=1, FILTER: TRUE
arrayMax :: Ugen -> Ugen
arrayMax array = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "ArrayMax" [] (Just [array]) 2 (Special 0) NoId

-- | detect the smallest value (and its position) in an array of Ugens
--
--  ArrayMin [ControlRate,AudioRate] *array=0;    MCE=1, FILTER: TRUE
arrayMin :: Ugen -> Ugen
arrayMin array = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "ArrayMin" [] (Just [array]) 2 (Special 0) NoId

-- | Sound Chip Simulator
--
--  Astrocade [AudioRate] reg0=0 reg1=127 reg2=0 reg3=0 reg4=0 reg5=0 reg6=15 reg7=0
astrocade :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
astrocade rate reg0 reg1 reg2 reg3 reg4 reg5 reg6 reg7 = mkUgen Nothing [AudioRate] (Left rate) "Astrocade" [reg0,reg1,reg2,reg3,reg4,reg5,reg6,reg7] Nothing 1 (Special 0) NoId

-- | TIA Chip Sound Simulator
--
--  Atari2600 [AudioRate] audc0=1 audc1=2 audf0=3 audf1=4 audv0=5 audv1=5 rate=1
atari2600 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
atari2600 rate audc0 audc1 audf0 audf1 audv0 audv1 rate_ = mkUgen Nothing [AudioRate] (Left rate) "Atari2600" [audc0,audc1,audf0,audf1,audv0,audv1,rate_] Nothing 1 (Special 0) NoId

-- | Use Amp data from a given partial
--
--  AtsAmp [ControlRate,AudioRate] atsbuffer=0 partialNum=0 filePointer=0
atsAmp :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
atsAmp rate atsbuffer partialNum filePointer = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "AtsAmp" [atsbuffer,partialNum,filePointer] Nothing 1 (Special 0) NoId

-- | (put short description here)
--
--  AtsBand [AudioRate] atsbuffer=0 band=0 filePointer=0
atsBand :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
atsBand rate atsbuffer band filePointer = mkUgen Nothing [AudioRate] (Left rate) "AtsBand" [atsbuffer,band,filePointer] Nothing 1 (Special 0) NoId

-- | Use Freq data from a given partial
--
--  AtsFreq [ControlRate,AudioRate] atsbuffer=0 partialNum=0 filePointer=0
atsFreq :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
atsFreq rate atsbuffer partialNum filePointer = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "AtsFreq" [atsbuffer,partialNum,filePointer] Nothing 1 (Special 0) NoId

-- | Resynthesize sine and noise data from an ATS analysis file
--
--  AtsNoiSynth [AudioRate] atsbuffer=0 numPartials=0 partialStart=0 partialSkip=1 filePointer=0 sinePct=1 noisePct=1 freqMul=1 freqAdd=0 numBands=25 bandStart=0 bandSkip=1
atsNoiSynth :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
atsNoiSynth rate atsbuffer numPartials partialStart partialSkip filePointer sinePct noisePct freqMul freqAdd numBands bandStart bandSkip = mkUgen Nothing [AudioRate] (Left rate) "AtsNoiSynth" [atsbuffer,numPartials,partialStart,partialSkip,filePointer,sinePct,noisePct,freqMul,freqAdd,numBands,bandStart,bandSkip] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AtsNoise [ControlRate,AudioRate] atsbuffer=0 bandNum=0 filePointer=0
atsNoise :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
atsNoise rate atsbuffer bandNum filePointer = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "AtsNoise" [atsbuffer,bandNum,filePointer] Nothing 1 (Special 0) NoId

-- | One Ugen to return both Amp and Freq info
--
--  AtsParInfo [ControlRate,AudioRate] atsbuffer=0 partialNum=0 filePointer=0
atsParInfo :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
atsParInfo rate atsbuffer partialNum filePointer = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "AtsParInfo" [atsbuffer,partialNum,filePointer] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  AtsPartial [AudioRate] atsbuffer=0 partial=0 filePointer=0 freqMul=1 freqAdd=0
atsPartial :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
atsPartial rate atsbuffer partial filePointer freqMul freqAdd = mkUgen Nothing [AudioRate] (Left rate) "AtsPartial" [atsbuffer,partial,filePointer,freqMul,freqAdd] Nothing 1 (Special 0) NoId

-- | Resynthesize sine data from an ATS analysis file
--
--  AtsSynth [AudioRate] atsbuffer=0 numPartials=0 partialStart=0 partialSkip=1 filePointer=0 freqMul=1 freqAdd=0
atsSynth :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
atsSynth rate atsbuffer numPartials partialStart partialSkip filePointer freqMul freqAdd = mkUgen Nothing [AudioRate] (Left rate) "AtsSynth" [atsbuffer,numPartials,partialStart,partialSkip,filePointer,freqMul,freqAdd] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  AtsUgen [] maxSize=0
atsUgen :: Rate -> Ugen -> Ugen
atsUgen rate maxSize = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "AtsUgen" [maxSize] Nothing 1 (Special 0) NoId

-- | Detect onsets and assess the nature of the attack slope
--
--  AttackSlope [ControlRate] input=0 windowsize=1024 peakpicksize=20 leak=0.999 energythreshold=0.01 sumthreshold=20 mingap=30 numslopesaveraged=10
attackSlope :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
attackSlope rate input windowsize peakpicksize leak energythreshold sumthreshold mingap numslopesaveraged = mkUgen Nothing [ControlRate] (Left rate) "AttackSlope" [input,windowsize,peakpicksize,leak,energythreshold,sumthreshold,mingap,numslopesaveraged] Nothing 6 (Special 0) NoId

-- | (Undocumented class)
--
--  AudioMSG [AudioRate] in=0 index=0;    FILTER: TRUE
audioMSG :: Ugen -> Ugen -> Ugen
audioMSG in_ index_ = mkUgen Nothing [AudioRate] (Right [0]) "AudioMSG" [in_,index_] Nothing 1 (Special 0) NoId

-- | calculates mean average of audio or control rate signal
--
--  AverageOutput [ControlRate,AudioRate] in=0 trig=0;    FILTER: TRUE
averageOutput :: Ugen -> Ugen -> Ugen
averageOutput in_ trig_ = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "AverageOutput" [in_,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  B2A [AudioRate] w=0 x=0 y=0 z=0
b2a :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
b2a rate w x y z = mkUgen Nothing [AudioRate] (Left rate) "B2A" [w,x,y,z] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  B2Ster [AudioRate] w=0 x=0 y=0
b2Ster :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
b2Ster rate w x y = mkUgen Nothing [AudioRate] (Left rate) "B2Ster" [w,x,y] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  B2UHJ [AudioRate] w=0 x=0 y=0
b2uhj :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
b2uhj rate w x y = mkUgen Nothing [AudioRate] (Left rate) "B2UHJ" [w,x,y] Nothing 2 (Special 0) NoId

-- | MultiOut BetaBlocker VChip
--
--  BBlockerBuf [AudioRate] freq=0 bufnum=0 startpoint=0
bBlockerBuf :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
bBlockerBuf rate freq bufnum startpoint = mkUgen Nothing [AudioRate] (Left rate) "BBlockerBuf" [freq,bufnum,startpoint] Nothing 9 (Special 0) NoId

-- | 3D Ambisonic decoder
--
--  BFDecode1 [AudioRate] w=0 x=0 y=0 z=0 azimuth=0 elevation=0 wComp=0
bfDecode1 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bfDecode1 rate w x y z azimuth elevation wComp = mkUgen Nothing [AudioRate] (Left rate) "BFDecode1" [w,x,y,z,azimuth,elevation,wComp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BFDecoder [] maxSize=0
bfDecoder :: Rate -> Ugen -> Ugen
bfDecoder rate maxSize = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "BFDecoder" [maxSize] Nothing 1 (Special 0) NoId

-- | Ambisonic B format encoder
--
--  BFEncode1 [AudioRate] in=0 azimuth=0 elevation=0 rho=1 gain=1 wComp=0
bfEncode1 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bfEncode1 rate in_ azimuth elevation rho gain wComp = mkUgen Nothing [AudioRate] (Left rate) "BFEncode1" [in_,azimuth,elevation,rho,gain,wComp] Nothing 4 (Special 0) NoId

-- | Ambisonic B format encoder
--
--  BFEncode2 [AudioRate] in=0 point_x=1 point_y=1 elevation=0 gain=1 wComp=0
bfEncode2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bfEncode2 rate in_ point_x point_y elevation gain wComp = mkUgen Nothing [AudioRate] (Left rate) "BFEncode2" [in_,point_x,point_y,elevation,gain,wComp] Nothing 4 (Special 0) NoId

-- | Ambisonic B format encoder for stereo signals
--
--  BFEncodeSter [AudioRate] l=0 r=0 azimuth=0 width=1.5708 elevation=0 rho=1 gain=1 wComp=0
bfEncodeSter :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bfEncodeSter rate l r azimuth width elevation rho gain wComp = mkUgen Nothing [AudioRate] (Left rate) "BFEncodeSter" [l,r,azimuth,width,elevation,rho,gain,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  BFGrainPanner [] maxSize=0
bfGrainPanner :: Rate -> Ugen -> Ugen
bfGrainPanner rate maxSize = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "BFGrainPanner" [maxSize] Nothing 1 (Special 0) NoId

-- | BFormat sound manipulation
--
--  BFManipulate [AudioRate] w=0 x=0 y=0 z=0 rotate=0 tilt=0 tumble=0
bfManipulate :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bfManipulate rate w x y z rotate_ tilt_ tumble_ = mkUgen Nothing [AudioRate] (Left rate) "BFManipulate" [w,x,y,z,rotate_,tilt_,tumble_] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  BFPanner [] maxSize=0
bfPanner :: Rate -> Ugen -> Ugen
bfPanner rate maxSize = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "BFPanner" [maxSize] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BLBufRd [ControlRate,AudioRate] bufnum=0 phase=0 ratio=1
blBufRd :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
blBufRd rate bufnum phase ratio = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "BLBufRd" [bufnum,phase,ratio] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BLOsc [ControlRate,AudioRate] freq=100 pulsewidth=0.5 waveform=0
blOsc :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
blOsc rate freq pulsewidth waveform = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "BLOsc" [freq,pulsewidth,waveform] Nothing 1 (Special 0) NoId

-- | 24db/oct rolloff - 4nd order resonant Low/High/Band Pass Filter
--
--  BMoog [AudioRate] in=0 freq=440 q=0.2 mode=0 saturation=0.95;    FILTER: TRUE
bMoog :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bMoog in_ freq q mode saturation = mkUgen Nothing [AudioRate] (Right [0]) "BMoog" [in_,freq,q,mode,saturation] Nothing 1 (Special 0) NoId

-- | Balances two signals with each other
--
--  Balance [AudioRate] in=0 test=0 hp=10 stor=0
balance :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
balance rate in_ test hp stor = mkUgen Nothing [AudioRate] (Left rate) "Balance" [in_,test,hp,stor] Nothing 1 (Special 0) NoId

-- | Extracts statistics on a beat histogram
--
--  BeatStatistics [ControlRate] fft=0 leak=0.995 numpreviousbeats=4
beatStatistics :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
beatStatistics rate fft_ leak numpreviousbeats = mkUgen Nothing [ControlRate] (Left rate) "BeatStatistics" [fft_,leak,numpreviousbeats] Nothing 4 (Special 0) NoId

-- | Sound Chip Simulator (well...)
--
--  Beep [AudioRate] freq=3250 vol=1
beep :: Rate -> Ugen -> Ugen -> Ugen
beep rate freq vol = mkUgen Nothing [AudioRate] (Left rate) "Beep" [freq,vol] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BeepU [] maxSize=0
beepU ::  Rate -> Ugen -> Ugen
beepU rate maxSize = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "BeepU" [maxSize] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BinData [ControlRate,AudioRate] buffer=0 bin=0 overlaps=0.5
binData :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
binData rate buffer bin overlaps = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "BinData" [buffer,bin,overlaps] Nothing 2 (Special 0) NoId

-- | Band limited impulse generation
--
--  BlitB3 [AudioRate] freq=440
blitB3 :: Rate -> Ugen -> Ugen
blitB3 rate freq = mkUgen Nothing [AudioRate] (Left rate) "BlitB3" [freq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BlitB3D [AudioRate] freq=440
blitB3D :: Rate -> Ugen -> Ugen
blitB3D rate freq = mkUgen Nothing [AudioRate] (Left rate) "BlitB3D" [freq] Nothing 1 (Special 0) NoId

-- | BLIT derived sawtooth
--
--  BlitB3Saw [AudioRate] freq=440 leak=0.99
blitB3Saw :: Rate -> Ugen -> Ugen -> Ugen
blitB3Saw rate freq leak = mkUgen Nothing [AudioRate] (Left rate) "BlitB3Saw" [freq,leak] Nothing 1 (Special 0) NoId

-- | Bipolar BLIT derived square waveform
--
--  BlitB3Square [AudioRate] freq=440 leak=0.99
blitB3Square :: Rate -> Ugen -> Ugen -> Ugen
blitB3Square rate freq leak = mkUgen Nothing [AudioRate] (Left rate) "BlitB3Square" [freq,leak] Nothing 1 (Special 0) NoId

-- | Bipolar BLIT derived triangle
--
--  BlitB3Tri [AudioRate] freq=440 leak=0.99 leak2=0.99
blitB3Tri :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
blitB3Tri rate freq leak leak2 = mkUgen Nothing [AudioRate] (Left rate) "BlitB3Tri" [freq,leak,leak2] Nothing 1 (Special 0) NoId

-- | breakcore simulator
--
--  Breakcore [AudioRate] bufnum=0 capturein=0 capturetrigger=0 duration=0.1 ampdropout=0
breakcore :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
breakcore rate bufnum capturein capturetrigger duration ampdropout = mkUgen Nothing [AudioRate] (Left rate) "Breakcore" [bufnum,capturein,capturetrigger,duration,ampdropout] Nothing 1 (Special 0) NoId

-- | Prigogine oscillator
--
--  Brusselator [AudioRate] reset=0 rate=0.01 mu=1 gamma=1 initx=0.5 inity=0.5
brusselator :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
brusselator rate reset rate_ mu gamma initx inity = mkUgen Nothing [AudioRate] (Left rate) "Brusselator" [reset,rate_,mu,gamma,initx,inity] Nothing 2 (Special 0) NoId

-- | Granular synthesis with sound sampled in a buffer
--
--  BufGrain [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 interp=2
bufGrain :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bufGrain rate trigger dur sndbuf rate_ pos interp = mkUgen Nothing [AudioRate] (Left rate) "BufGrain" [trigger,dur,sndbuf,rate_,pos,interp] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sound sampled in a buffer and user supplied envelope
--
--  BufGrainB [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 envbuf=0 interp=2
bufGrainB :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bufGrainB rate trigger dur sndbuf rate_ pos envbuf interp = mkUgen Nothing [AudioRate] (Left rate) "BufGrainB" [trigger,dur,sndbuf,rate_,pos,envbuf,interp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BufGrainBBF [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 envbuf=0 azimuth=0 elevation=0 rho=1 interp=2 wComp=0
bufGrainBBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bufGrainBBF rate trigger dur sndbuf rate_ pos envbuf azimuth elevation rho interp wComp = mkUgen Nothing [AudioRate] (Left rate) "BufGrainBBF" [trigger,dur,sndbuf,rate_,pos,envbuf,azimuth,elevation,rho,interp,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  BufGrainBF [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 azimuth=0 elevation=0 rho=1 interp=2 wComp=0
bufGrainBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bufGrainBF rate trigger dur sndbuf rate_ pos azimuth elevation rho interp wComp = mkUgen Nothing [AudioRate] (Left rate) "BufGrainBF" [trigger,dur,sndbuf,rate_,pos,azimuth,elevation,rho,interp,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with sound sampled in a buffer and user supplied envelopes
--
--  BufGrainI [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 envbuf1=0 envbuf2=0 ifac=0.5 interp=2
bufGrainI :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bufGrainI rate trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac interp = mkUgen Nothing [AudioRate] (Left rate) "BufGrainI" [trigger,dur,sndbuf,rate_,pos,envbuf1,envbuf2,ifac,interp] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  BufGrainIBF [AudioRate] trigger=0 dur=1 sndbuf=0 rate=1 pos=0 envbuf1=0 envbuf2=0 ifac=0.5 azimuth=0 elevation=0 rho=1 interp=2 wComp=0
bufGrainIBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bufGrainIBF rate trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac azimuth elevation rho interp wComp = mkUgen Nothing [AudioRate] (Left rate) "BufGrainIBF" [trigger,dur,sndbuf,rate_,pos,envbuf1,envbuf2,ifac,azimuth,elevation,rho,interp,wComp] Nothing 4 (Special 0) NoId

-- | detect the largest value (and its position) in an array of Ugens
--
--  BufMax [ControlRate] bufnum=0 gate=1
bufMax :: Rate -> Ugen -> Ugen -> Ugen
bufMax rate bufnum gate_ = mkUgen Nothing [ControlRate] (Left rate) "BufMax" [bufnum,gate_] Nothing 2 (Special 0) NoId

-- | detect the largest value (and its position) in an array of Ugens
--
--  BufMin [ControlRate] bufnum=0 gate=1
bufMin :: Rate -> Ugen -> Ugen -> Ugen
bufMin rate bufnum gate_ = mkUgen Nothing [ControlRate] (Left rate) "BufMin" [bufnum,gate_] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  CQ_Diff [ControlRate] in1=0 in2=0 databufnum=0
cq_Diff :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
cq_Diff rate in1 in2 databufnum = mkUgen Nothing [ControlRate] (Left rate) "CQ_Diff" [in1,in2,databufnum] Nothing 1 (Special 0) NoId

-- | Quefrency analysis and liftering
--
--  Cepstrum [] cepbuf=0 fftchain=0
cepstrum :: Rate -> Ugen -> Ugen -> Ugen
cepstrum rate cepbuf fftchain = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "Cepstrum" [cepbuf,fftchain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Chen [ControlRate,AudioRate] speed=0.5 a=0.5 b=0.3 c=0.28
chen :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
chen rate speed a b c = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Chen" [speed,a,b,c] Nothing 3 (Special 0) NoId

-- | Octave chroma band based representation of energy in a signal; Chromagram for nTET tuning systems with any base reference
--
--  Chromagram [ControlRate] fft=0 fftsize=2048 n=12 tuningbase=32.7032 octaves=8 integrationflag=0 coeff=0.9 octaveratio=2 perframenormalize=0
chromagram :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
chromagram rate fft_ fftsize n tuningbase octaves integrationflag coeff octaveratio perframenormalize = mkUgen Nothing [ControlRate] (Left rate) "Chromagram" [fft_,fftsize,n,tuningbase,octaves,integrationflag,coeff,octaveratio,perframenormalize] Nothing 12 (Special 0) NoId

-- | circular linear lag
--
--  CircleRamp [ControlRate,AudioRate] in=0 lagTime=0.1 circmin=-180 circmax=180
circleRamp :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
circleRamp rate in_ lagTime circmin circmax = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "CircleRamp" [in_,lagTime,circmin,circmax] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clipper32 [AudioRate] in=0 lo=-0.8 hi=0.8
clipper32 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
clipper32 rate in_ lo hi = mkUgen Nothing [AudioRate] (Left rate) "Clipper32" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clipper4 [AudioRate] in=0 lo=-0.8 hi=0.8
clipper4 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
clipper4 rate in_ lo hi = mkUgen Nothing [AudioRate] (Left rate) "Clipper4" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clipper8 [AudioRate] in=0 lo=-0.8 hi=0.8
clipper8 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
clipper8 rate in_ lo hi = mkUgen Nothing [AudioRate] (Left rate) "Clipper8" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Clockmus [ControlRate]
clockmus :: Rate -> Ugen
clockmus rate = mkUgen Nothing [ControlRate] (Left rate) "Clockmus" [] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  CombLP [AudioRate] in=0 gate=1 maxdelaytime=0.2 delaytime=0.2 decaytime=1 coef=0.5
combLP :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
combLP rate in_ gate_ maxdelaytime delaytime decaytime coef = mkUgen Nothing [AudioRate] (Left rate) "CombLP" [in_,gate_,maxdelaytime,delaytime,decaytime,coef] Nothing 1 (Special 0) NoId

-- | FM-modulable resonating filter
--
--  ComplexRes [AudioRate] in=0 freq=100 decay=0.2;    FILTER: TRUE
complexRes :: Ugen -> Ugen -> Ugen -> Ugen
complexRes in_ freq decay_ = mkUgen Nothing [AudioRate] (Right [0]) "ComplexRes" [in_,freq,decay_] Nothing 1 (Special 0) NoId

-- | Concatenative Cross-Synthesis on Live Streams
--
--  Concat [AudioRate] control=0 source=0 storesize=1 seektime=1 seekdur=1 matchlength=0.05 freezestore=0 zcr=1 lms=1 sc=1 st=0 randscore=0
concat :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
concat rate control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore = mkUgen Nothing [AudioRate] (Left rate) "Concat" [control_,source,storesize,seektime,seekdur,matchlength,freezestore,zcr,lms,sc,st,randscore] Nothing 1 (Special 0) NoId

-- | Concatenative Cross-Synthesis on Live Streams
--
--  Concat2 [AudioRate] control=0 source=0 storesize=1 seektime=1 seekdur=1 matchlength=0.05 freezestore=0 zcr=1 lms=1 sc=1 st=0 randscore=0 threshold=0.01
concat2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
concat2 rate control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore threshold = mkUgen Nothing [AudioRate] (Left rate) "Concat2" [control_,source,storesize,seektime,seekdur,matchlength,freezestore,zcr,lms,sc,st,randscore,threshold] Nothing 1 (Special 0) NoId

-- | an amplitude tracking based onset detector
--
--  Coyote [ControlRate] in=0 trackFall=0.2 slowLag=0.2 fastLag=0.01 fastMul=0.5 thresh=0.05 minDur=0.1
coyote :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
coyote rate in_ trackFall slowLag fastLag fastMul thresh minDur = mkUgen Nothing [ControlRate] (Left rate) "Coyote" [in_,trackFall,slowLag,fastLag,fastMul,thresh,minDur] Nothing 1 (Special 0) NoId

-- | Measure the temporal crest factor of a signal
--
--  Crest [ControlRate] in=0 numsamps=400 gate=1
crest :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
crest rate in_ numsamps gate_ = mkUgen Nothing [ControlRate] (Left rate) "Crest" [in_,numsamps,gate_] Nothing 1 (Special 0) NoId

-- | class B/AB power amp distortion simulation
--
--  CrossoverDistortion [AudioRate] in=0 amp=0.5 smooth=0.5;    FILTER: TRUE
crossoverDistortion :: Ugen -> Ugen -> Ugen -> Ugen
crossoverDistortion in_ amp smooth = mkUgen Nothing [AudioRate] (Right [0]) "CrossoverDistortion" [in_,amp,smooth] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  DCompressor [AudioRate] input=0 sidechainIn=0 sidechain=0 ratio=4 threshold=-40 attack=0.1 release=100.1 makeup=0.5 automakeup=1;    FILTER: TRUE
dCompressor :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dCompressor input sidechainIn sidechain ratio threshold attack release makeup automakeup = mkUgen Nothing [AudioRate] (Right [0]) "DCompressor" [input,sidechainIn,sidechain,ratio,threshold,attack,release,makeup,automakeup] Nothing 1 (Special 0) NoId

-- | Digitally modelled analog filter
--
--  DFM1 [AudioRate] in=0 freq=1000 res=0.1 inputgain=1 type=0 noiselevel=0.0003;    FILTER: TRUE
dfm1 :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dfm1 in_ freq res inputgain type_ noiselevel = mkUgen Nothing [AudioRate] (Right [0]) "DFM1" [in_,freq,res,inputgain,type_,noiselevel] Nothing 1 (Special 0) NoId

-- | Demand rate implementation of a Wiard noise ring
--
--  DNoiseRing [DemandRate] change=0.5 chance=0.5 shift=1 numBits=8 resetval=0;    DEMAND/NONDET
dNoiseRingId :: ID a => a -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dNoiseRingId z change chance shift numBits resetval = mkUgen Nothing [DemandRate] (Left DemandRate) "DNoiseRing" [change,chance,shift,numBits,resetval] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of DNoiseRing.
dNoiseRingM :: Uid m => Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
dNoiseRingM = liftUid5 dNoiseRingId

-- | Unsafe variant of DNoiseRing.
dNoiseRing ::  Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dNoiseRing = liftUnsafe5 dNoiseRingM

-- | Triangle via 3rd order differerentiated polynomial waveform
--
--  DPW3Tri [AudioRate] freq=440
dpw3Tri :: Rate -> Ugen -> Ugen
dpw3Tri rate freq = mkUgen Nothing [AudioRate] (Left rate) "DPW3Tri" [freq] Nothing 1 (Special 0) NoId

-- | Sawtooth via 4th order differerentiated polynomial waveform
--
--  DPW4Saw [AudioRate] freq=440
dpw4Saw :: Rate -> Ugen -> Ugen
dpw4Saw rate freq = mkUgen Nothing [AudioRate] (Left rate) "DPW4Saw" [freq] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGBowed [AudioRate] freq=440 velb=0.5 force=1 gate=1 pos=0.14 release=0.1 c1=1 c3=3 impZ=0.55 fB=2
dwgBowed :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dwgBowed rate freq velb force gate_ pos release c1 c3 impZ fB = mkUgen Nothing [AudioRate] (Left rate) "DWGBowed" [freq,velb,force,gate_,pos,release,c1,c3,impZ,fB] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGBowedSimple [AudioRate] freq=440 velb=0.5 force=1 gate=1 pos=0.14 release=0.1 c1=1 c3=30
dwgBowedSimple :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dwgBowedSimple rate freq velb force gate_ pos release c1 c3 = mkUgen Nothing [AudioRate] (Left rate) "DWGBowedSimple" [freq,velb,force,gate_,pos,release,c1,c3] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGBowedTor [AudioRate] freq=440 velb=0.5 force=1 gate=1 pos=0.14 release=0.1 c1=1 c3=3 impZ=0.55 fB=2 mistune=5.2 c1tor=1 c3tor=3000 iZtor=1.8
dwgBowedTor :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dwgBowedTor rate freq velb force gate_ pos release c1 c3 impZ fB mistune c1tor c3tor iZtor = mkUgen Nothing [AudioRate] (Left rate) "DWGBowedTor" [freq,velb,force,gate_,pos,release,c1,c3,impZ,fB,mistune,c1tor,c3tor,iZtor] Nothing 1 (Special 0) NoId

-- | Clarinet physical model.
--
--  DWGClarinet3 [AudioRate] freq=440.0 pm=1.0 pc=1.0 m=0.8 gate=1.0 release=1.0e-2 c1=0.25 c3=7.0
dwgClarinet3 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dwgClarinet3 rate freq pm pc m gate_ release c1 c3 = mkUgen Nothing [AudioRate] (Left rate) "DWGClarinet3" [freq,pm,pc,m,gate_,release,c1,c3] Nothing 1 (Special 0) NoId

-- | Reimplementation of STK flute model.
--
--  DWGFlute [AudioRate] freq=400.0 pm=1.0 endr=0.5 jetr=0.25 jetRa=0.33 gate=1.0 release=0.1
dwgFlute :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dwgFlute rate freq pm endr jetr jetRa gate_ release = mkUgen Nothing [AudioRate] (Left rate) "DWGFlute" [freq,pm,endr,jetr,jetRa,gate_,release] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGPlucked [AudioRate] freq=440 amp=0.5 gate=1 pos=0.14 c1=1 c3=30 inp=0 release=0.1
dwgPlucked :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dwgPlucked rate freq amp gate_ pos c1 c3 inp release = mkUgen Nothing [AudioRate] (Left rate) "DWGPlucked" [freq,amp,gate_,pos,c1,c3,inp,release] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGPlucked2 [AudioRate] freq=440 amp=0.5 gate=1 pos=0.14 c1=1 c3=30 inp=0 release=0.1 mistune=1.008 mp=0.55 gc=0.01
dwgPlucked2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dwgPlucked2 rate freq amp gate_ pos c1 c3 inp release mistune mp gc = mkUgen Nothing [AudioRate] (Left rate) "DWGPlucked2" [freq,amp,gate_,pos,c1,c3,inp,release,mistune,mp,gc] Nothing 1 (Special 0) NoId

-- | Plucked physical model.
--
--  DWGPluckedStiff [AudioRate] freq=440 amp=0.5 gate=1 pos=0.14 c1=1 c3=30 inp=0 release=0.1 fB=2
dwgPluckedStiff :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dwgPluckedStiff rate freq amp gate_ pos c1 c3 inp release fB = mkUgen Nothing [AudioRate] (Left rate) "DWGPluckedStiff" [freq,amp,gate_,pos,c1,c3,inp,release,fB] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  DWGSoundBoard [AudioRate] inp=0 c1=20 c3=20 mix=0.8 d1=199 d2=211 d3=223 d4=227 d5=229 d6=233 d7=239 d8=241;    FILTER: TRUE
dwgSoundBoard :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dwgSoundBoard inp c1 c3 mix d1 d2 d3 d4 d5 d6 d7 d8 = mkUgen Nothing [AudioRate] (Right [0]) "DWGSoundBoard" [inp,c1,c3,mix,d1,d2,d3,d4,d5,d6,d7,d8] Nothing 1 (Special 0) NoId

-- | demand rate brownian movement with Gendyn distributions
--
--  Dbrown2 [] lo=0 hi=0 step=0 dist=0 length=100000000
dbrown2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dbrown2 rate lo hi step dist length_ = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "Dbrown2" [lo,hi,step,dist,length_] Nothing 1 (Special 0) NoId

-- | demand rate tag system on a buffer
--
--  DbufTag [DemandRate] bufnum=0 v=0 axiom=0 rules=0 recycle=0 mode=0;    DEMAND/NONDET
dbufTagId :: ID a => a -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dbufTagId z bufnum v axiom rules recycle mode = mkUgen Nothing [DemandRate] (Left DemandRate) "DbufTag" [bufnum,v,axiom,rules,recycle,mode] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of DbufTag.
dbufTagM :: Uid m => Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
dbufTagM = liftUid6 dbufTagId

-- | Unsafe variant of DbufTag.
dbufTag ::  Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dbufTag = liftUnsafe6 dbufTagM

-- | Samplerate and bitrate reduction
--
--  Decimator [AudioRate] in=0 rate=44100 bits=24
decimator :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
decimator rate in_ rate_ bits = mkUgen Nothing [AudioRate] (Left rate) "Decimator" [in_,rate_,bits] Nothing 1 (Special 0) NoId

-- | Demand version of the BetaBlocker VChip
--
--  DetaBlockerBuf [DemandRate] bufnum=0 startpoint=0;    DEMAND/NONDET
detaBlockerBufId :: ID a => a -> Ugen -> Ugen -> Ugen
detaBlockerBufId z bufnum startpoint = mkUgen Nothing [DemandRate] (Left DemandRate) "DetaBlockerBuf" [bufnum,startpoint] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of DetaBlockerBuf.
detaBlockerBufM :: Uid m => Ugen -> Ugen -> m Ugen
detaBlockerBufM = liftUid2 detaBlockerBufId

-- | Unsafe variant of DetaBlockerBuf.
detaBlockerBuf ::  Ugen -> Ugen -> Ugen
detaBlockerBuf = liftUnsafe2 detaBlockerBufM

-- | demand rate finite state machine
--
--  Dfsm [DemandRate] rules=0 n=1 rgen=0;    DEMAND/NONDET
dfsmId :: ID a => a -> Ugen -> Ugen -> Ugen -> Ugen
dfsmId z rules n rgen = mkUgen Nothing [DemandRate] (Left DemandRate) "Dfsm" [rules,n,rgen] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of Dfsm.
dfsmM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
dfsmM = liftUid3 dfsmId

-- | Unsafe variant of Dfsm.
dfsm ::  Ugen -> Ugen -> Ugen -> Ugen
dfsm = liftUnsafe3 dfsmM

-- | (Undocumented class)
--
--  Dgauss [] lo=0 hi=0 length=100000000
dgauss :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
dgauss rate lo hi length_ = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "Dgauss" [lo,hi,length_] Nothing 1 (Special 0) NoId

-- | Ring modulation based on the physical model of a diode.
--
--  DiodeRingMod [AudioRate] car=0 mod=0;    FILTER: TRUE
diodeRingMod :: Ugen -> Ugen -> Ugen
diodeRingMod car mod_ = mkUgen Nothing [AudioRate] (Right [0]) "DiodeRingMod" [car,mod_] Nothing 1 (Special 0) NoId

-- | port of some ladspa plugins
--
--  Disintegrator [AudioRate] in=0 probability=0.5 multiplier=0;    FILTER: TRUE, NONDET
disintegratorId :: ID a => a -> Ugen -> Ugen -> Ugen -> Ugen
disintegratorId z in_ probability multiplier = mkUgen Nothing [AudioRate] (Right [0]) "Disintegrator" [in_,probability,multiplier] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of Disintegrator.
disintegratorM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
disintegratorM = liftUid3 disintegratorId

-- | Unsafe variant of Disintegrator.
disintegrator ::  Ugen -> Ugen -> Ugen -> Ugen
disintegrator = liftUnsafe3 disintegratorM

-- | discrete time neurodynamics
--
--  Dneuromodule [ControlRate,AudioRate,DemandRate] dt=0 *theta=0 *x=0 *weights=0;    MCE=3, NC INPUT: True, NONDET
dneuromoduleId :: ID a => Int -> a -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dneuromoduleId numChannels z dt theta x weights = mkUgen Nothing [ControlRate,AudioRate,DemandRate] (Left DemandRate) "Dneuromodule" [dt] (Just [theta,x,weights]) numChannels (Special 0) (toUid z)

-- | Monad variant of Dneuromodule.
dneuromoduleM :: Uid m => Int -> Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
dneuromoduleM = liftUid5 dneuromoduleId

-- | Unsafe variant of Dneuromodule.
dneuromodule ::  Int -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dneuromodule = liftUnsafe5 dneuromoduleM

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  DoubleNestedAllpassC [AudioRate] in=0 maxdelay1=0.0047 delay1=0.0047 gain1=0.15 maxdelay2=0.022 delay2=0.022 gain2=0.25 maxdelay3=0.0083 delay3=0.0083 gain3=0.3;    FILTER: TRUE
doubleNestedAllpassC :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
doubleNestedAllpassC in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 maxdelay3 delay3 gain3 = mkUgen Nothing [AudioRate] (Right [0]) "DoubleNestedAllpassC" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2,maxdelay3,delay3,gain3] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  DoubleNestedAllpassL [AudioRate] in=0 maxdelay1=0.0047 delay1=0.0047 gain1=0.15 maxdelay2=0.022 delay2=0.022 gain2=0.25 maxdelay3=0.0083 delay3=0.0083 gain3=0.3;    FILTER: TRUE
doubleNestedAllpassL :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
doubleNestedAllpassL in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 maxdelay3 delay3 gain3 = mkUgen Nothing [AudioRate] (Right [0]) "DoubleNestedAllpassL" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2,maxdelay3,delay3,gain3] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  DoubleNestedAllpassN [AudioRate] in=0 maxdelay1=0.0047 delay1=0.0047 gain1=0.15 maxdelay2=0.022 delay2=0.022 gain2=0.25 maxdelay3=0.0083 delay3=0.0083 gain3=0.3;    FILTER: TRUE
doubleNestedAllpassN :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
doubleNestedAllpassN in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 maxdelay3 delay3 gain3 = mkUgen Nothing [AudioRate] (Right [0]) "DoubleNestedAllpassN" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2,maxdelay3,delay3,gain3] Nothing 1 (Special 0) NoId

-- | Forced DoubleWell Oscillator
--
--  DoubleWell [AudioRate] reset=0 ratex=0.01 ratey=0.01 f=1 w=0.001 delta=1 initx=0 inity=0
doubleWell :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
doubleWell rate reset ratex ratey f w delta initx inity = mkUgen Nothing [AudioRate] (Left rate) "DoubleWell" [reset,ratex,ratey,f,w,delta,initx,inity] Nothing 1 (Special 0) NoId

-- | Forced DoubleWell Oscillator
--
--  DoubleWell2 [AudioRate] reset=0 ratex=0.01 ratey=0.01 f=1 w=0.001 delta=1 initx=0 inity=0
doubleWell2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
doubleWell2 rate reset ratex ratey f w delta initx inity = mkUgen Nothing [AudioRate] (Left rate) "DoubleWell2" [reset,ratex,ratey,f,w,delta,initx,inity] Nothing 1 (Special 0) NoId

-- | Forced DoubleWell Oscillator
--
--  DoubleWell3 [AudioRate] reset=0 rate=0.01 f=0 delta=0.25 initx=0 inity=0
doubleWell3 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
doubleWell3 rate reset rate_ f delta initx inity = mkUgen Nothing [AudioRate] (Left rate) "DoubleWell3" [reset,rate_,f,delta,initx,inity] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  DriveNoise [AudioRate] in=0 amount=1 multi=5
driveNoise :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
driveNoise rate in_ amount multi = mkUgen Nothing [AudioRate] (Left rate) "DriveNoise" [in_,amount,multi] Nothing 1 (Special 0) NoId

-- | Crosscorrelation search and drum pattern matching beat tracker
--
--  DrumTrack [ControlRate] in=0 lock=0 dynleak=0 tempowt=0 phasewt=0 basswt=0 patternwt=1 prior=0 kicksensitivity=1 snaresensitivity=1 debugmode=0
drumTrack :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
drumTrack rate in_ lock dynleak tempowt phasewt basswt patternwt prior kicksensitivity snaresensitivity debugmode = mkUgen Nothing [ControlRate] (Left rate) "DrumTrack" [in_,lock,dynleak,tempowt,phasewt,basswt,patternwt,prior,kicksensitivity,snaresensitivity,debugmode] Nothing 4 (Special 0) NoId

-- | demand rate tag system
--
--  Dtag [] bufsize=0 v=0 axiom=0 rules=0 recycle=0 mode=0
dtag :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dtag rate bufsize v axiom rules recycle mode = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "Dtag" [bufsize,v,axiom,rules,recycle,mode] Nothing 1 (Special 0) NoId

-- | Envelope Follower Filter
--
--  EnvDetect [AudioRate] in=0 attack=100 release=0
envDetect :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
envDetect rate in_ attack release = mkUgen Nothing [AudioRate] (Left rate) "EnvDetect" [in_,attack,release] Nothing 1 (Special 0) NoId

-- | Envelope Follower
--
--  EnvFollow [ControlRate,AudioRate] input=0 decaycoeff=0.99
envFollow :: Rate -> Ugen -> Ugen -> Ugen
envFollow rate input decaycoeff = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "EnvFollow" [input,decaycoeff] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTComplexDev [ControlRate] buffer=0 rectify=0 powthresh=0.1
fftComplexDev :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
fftComplexDev rate buffer rectify powthresh = mkUgen Nothing [ControlRate] (Left rate) "FFTComplexDev" [buffer,rectify,powthresh] Nothing 1 (Special 0) NoId

-- | Spectral crest measure
--
--  FFTCrest [ControlRate] buffer=0 freqlo=0 freqhi=50000
fftCrest :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
fftCrest rate buffer freqlo freqhi = mkUgen Nothing [ControlRate] (Left rate) "FFTCrest" [buffer,freqlo,freqhi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTDiffMags [ControlRate] bufferA=0 bufferB=0
fftDiffMags :: Rate -> Ugen -> Ugen -> Ugen
fftDiffMags rate bufferA bufferB = mkUgen Nothing [ControlRate] (Left rate) "FFTDiffMags" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTFlux [ControlRate] buffer=0 normalise=1
fftFlux :: Rate -> Ugen -> Ugen -> Ugen
fftFlux rate buffer normalise = mkUgen Nothing [ControlRate] (Left rate) "FFTFlux" [buffer,normalise] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTFluxPos [ControlRate] buffer=0 normalise=1
fftFluxPos :: Rate -> Ugen -> Ugen -> Ugen
fftFluxPos rate buffer normalise = mkUgen Nothing [ControlRate] (Left rate) "FFTFluxPos" [buffer,normalise] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTMKL [ControlRate] buffer=0 epsilon=0.0
fftmkl :: Rate -> Ugen -> Ugen -> Ugen
fftmkl rate buffer epsilon = mkUgen Nothing [ControlRate] (Left rate) "FFTMKL" [buffer,epsilon] Nothing 1 (Special 0) NoId

-- | Find peak value in an FFT frame
--
--  FFTPeak [ControlRate] buffer=0 freqlo=0 freqhi=50000
fftPeak :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
fftPeak rate buffer freqlo freqhi = mkUgen Nothing [ControlRate] (Left rate) "FFTPeak" [buffer,freqlo,freqhi] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTPhaseDev [ControlRate] buffer=0 weight=0 powthresh=0.1
fftPhaseDev :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
fftPhaseDev rate buffer weight powthresh = mkUgen Nothing [ControlRate] (Left rate) "FFTPhaseDev" [buffer,weight,powthresh] Nothing 1 (Special 0) NoId

-- | Instantaneous spectral power
--
--  FFTPower [ControlRate] buffer=0 square=1
fftPower :: Rate -> Ugen -> Ugen -> Ugen
fftPower rate buffer square = mkUgen Nothing [ControlRate] (Left rate) "FFTPower" [buffer,square] Nothing 1 (Special 0) NoId

-- | Spectral slope
--
--  FFTSlope [ControlRate] buffer=0
fftSlope :: Rate -> Ugen -> Ugen
fftSlope rate buffer = mkUgen Nothing [ControlRate] (Left rate) "FFTSlope" [buffer] Nothing 1 (Special 0) NoId

-- | Spectral spread
--
--  FFTSpread [ControlRate] buffer=0 centroid=0
fftSpread :: Rate -> Ugen -> Ugen -> Ugen
fftSpread rate buffer centroid = mkUgen Nothing [ControlRate] (Left rate) "FFTSpread" [buffer,centroid] Nothing 1 (Special 0) NoId

-- | Spectral flatness, divided into subbands
--
--  FFTSubbandFlatness [ControlRate] chain=0 cutfreqs=0
fftSubbandFlatness :: Rate -> Ugen -> Ugen -> Ugen
fftSubbandFlatness rate chain cutfreqs = mkUgen Nothing [ControlRate] (Left rate) "FFTSubbandFlatness" [chain,cutfreqs] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FFTSubbandFlux [ControlRate] chain=0 cutfreqs=0 posonly=0
fftSubbandFlux :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
fftSubbandFlux rate chain cutfreqs posonly = mkUgen Nothing [ControlRate] (Left rate) "FFTSubbandFlux" [chain,cutfreqs,posonly] Nothing 1 (Special 0) NoId

-- | Spectral power, divided into subbands
--
--  FFTSubbandPower [ControlRate] chain=0 cutfreqs=0 square=1 scalemode=1
fftSubbandPower :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fftSubbandPower rate chain cutfreqs square scalemode = mkUgen Nothing [ControlRate] (Left rate) "FFTSubbandPower" [chain,cutfreqs,square,scalemode] Nothing 1 (Special 0) NoId

-- | Phase modulation oscillator matrix.
--
--  FM7 [AudioRate] *ctlMatrix=0 *modMatrix=0;    MCE=2
fm7 :: Rate -> Ugen -> Ugen -> Ugen
fm7 rate ctlMatrix modMatrix = mkUgen Nothing [AudioRate] (Left rate) "FM7" [] (Just [ctlMatrix,modMatrix]) 6 (Special 0) NoId

-- | Granular synthesis with FM grains
--
--  FMGrain [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1;    FILTER: TRUE
fmGrain :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fmGrain trigger dur carfreq modfreq index_ = mkUgen Nothing [AudioRate] (Right [0]) "FMGrain" [trigger,dur,carfreq,modfreq,index_] Nothing 1 (Special 0) NoId

-- | Granular synthesis with FM grains and user supplied envelope
--
--  FMGrainB [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 envbuf=0;    FILTER: TRUE
fmGrainB :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fmGrainB trigger dur carfreq modfreq index_ envbuf = mkUgen Nothing [AudioRate] (Right [0]) "FMGrainB" [trigger,dur,carfreq,modfreq,index_,envbuf] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FMGrainBBF [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 envbuf=0 azimuth=0 elevation=0 rho=1 wComp=0
fmGrainBBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fmGrainBBF rate trigger dur carfreq modfreq index_ envbuf azimuth elevation rho wComp = mkUgen Nothing [AudioRate] (Left rate) "FMGrainBBF" [trigger,dur,carfreq,modfreq,index_,envbuf,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  FMGrainBF [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 azimuth=0 elevation=0 rho=1 wComp=0
fmGrainBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fmGrainBF rate trigger dur carfreq modfreq index_ azimuth elevation rho wComp = mkUgen Nothing [AudioRate] (Left rate) "FMGrainBF" [trigger,dur,carfreq,modfreq,index_,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with FM grains and user supplied envelopes
--
--  FMGrainI [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 envbuf1=0 envbuf2=0 ifac=0.5
fmGrainI :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fmGrainI rate trigger dur carfreq modfreq index_ envbuf1 envbuf2 ifac = mkUgen Nothing [AudioRate] (Left rate) "FMGrainI" [trigger,dur,carfreq,modfreq,index_,envbuf1,envbuf2,ifac] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FMGrainIBF [AudioRate] trigger=0 dur=1 carfreq=440 modfreq=200 index=1 envbuf1=0 envbuf2=0 ifac=0.5 azimuth=0 elevation=0 rho=1 wComp=0
fmGrainIBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fmGrainIBF rate trigger dur carfreq modfreq index_ envbuf1 envbuf2 ifac azimuth elevation rho wComp = mkUgen Nothing [AudioRate] (Left rate) "FMGrainIBF" [trigger,dur,carfreq,modfreq,index_,envbuf1,envbuf2,ifac,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Decode an FMH signal for a specific speaker
--
--  FMHDecode1 [AudioRate] w=0 x=0 y=0 z=0 r=0 s=0 t=0 u=0 v=0 azimuth=0 elevation=0
fmhDecode1 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fmhDecode1 rate w x y z r s t u v azimuth elevation = mkUgen Nothing [AudioRate] (Left rate) "FMHDecode1" [w,x,y,z,r,s,t,u,v,azimuth,elevation] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FMHEncode0 [AudioRate] in=0 azimuth=0 elevation=0 gain=1
fmhEncode0 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fmhEncode0 rate in_ azimuth elevation gain = mkUgen Nothing [AudioRate] (Left rate) "FMHEncode0" [in_,azimuth,elevation,gain] Nothing 9 (Special 0) NoId

-- | Second Order Ambisonic encoder
--
--  FMHEncode1 [AudioRate] in=0 azimuth=0 elevation=0 rho=1 gain=1 wComp=0
fmhEncode1 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fmhEncode1 rate in_ azimuth elevation rho gain wComp = mkUgen Nothing [AudioRate] (Left rate) "FMHEncode1" [in_,azimuth,elevation,rho,gain,wComp] Nothing 9 (Special 0) NoId

-- | Second Order Ambisonic encoder
--
--  FMHEncode2 [AudioRate] in=0 point_x=0 point_y=0 elevation=0 gain=1 wComp=0
fmhEncode2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fmhEncode2 rate in_ point_x point_y elevation gain wComp = mkUgen Nothing [AudioRate] (Left rate) "FMHEncode2" [in_,point_x,point_y,elevation,gain,wComp] Nothing 9 (Special 0) NoId

-- | Storing feature data from Ugens in NRT mode
--
--  FeatureSave [ControlRate] features=0 trig=0
featureSave :: Rate -> Ugen -> Ugen -> Ugen
featureSave rate features trig_ = mkUgen Nothing [ControlRate] (Left rate) "FeatureSave" [features,trig_] Nothing 1 (Special 0) NoId

-- | FitzHughNagumo Neuron Firing Oscillator
--
--  Fhn2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0 u0=0 w0=0
fhn2DC :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fhn2DC rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Fhn2DC" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | FitzHughNagumo Neuron Firing Oscillator
--
--  Fhn2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0 u0=0 w0=0
fhn2DL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fhn2DL rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Fhn2DL" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | FitzHughNagumo Neuron Firing Oscillator
--
--  Fhn2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0 u0=0 w0=0
fhn2DN :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fhn2DN rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Fhn2DN" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FhnTrig [ControlRate,AudioRate] minfreq=4 maxfreq=10 urate=0.1 wrate=0.1 b0=0.6 b1=0.8 i=0 u0=0 w0=0
fhnTrig :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fhnTrig rate minfreq maxfreq urate wrate b0 b1 i u0 w0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "FhnTrig" [minfreq,maxfreq,urate,wrate,b0,b1,i,u0,w0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  FincoSprottL [AudioRate] freq=22050 a=2.45 h=0.05 xi=0 yi=0 zi=0
fincoSprottL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fincoSprottL rate freq a h xi yi zi = mkUgen Nothing [AudioRate] (Left rate) "FincoSprottL" [freq,a,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  FincoSprottM [AudioRate] freq=22050 a=-7 b=4 h=0.05 xi=0 yi=0 zi=0
fincoSprottM :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fincoSprottM rate freq a b h xi yi zi = mkUgen Nothing [AudioRate] (Left rate) "FincoSprottM" [freq,a,b,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  FincoSprottS [AudioRate] freq=22050 a=8 b=2 h=0.05 xi=0 yi=0 zi=0
fincoSprottS :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fincoSprottS rate freq a b h xi yi zi = mkUgen Nothing [AudioRate] (Left rate) "FincoSprottS" [freq,a,b,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | Neuron Firing Model Oscillator
--
--  FitzHughNagumo [AudioRate] reset=0 rateu=0.01 ratew=0.01 b0=1 b1=1 initu=0 initw=0
fitzHughNagumo :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fitzHughNagumo rate reset rateu ratew b0 b1 initu initw = mkUgen Nothing [AudioRate] (Left rate) "FitzHughNagumo" [reset,rateu,ratew,b0,b1,initu,initw] Nothing 1 (Special 0) NoId

-- | calculates spectral MSE distance of two fft chains
--
--  FrameCompare [ControlRate] buffer1=0 buffer2=0 wAmount=0.5
frameCompare :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
frameCompare rate buffer1 buffer2 wAmount = mkUgen Nothing [ControlRate] (Left rate) "FrameCompare" [buffer1,buffer2,wAmount] Nothing 1 (Special 0) NoId

-- | A physical model of a system with dry-friction. A chaotic filter.
--
--  Friction [ControlRate,AudioRate] in=0 friction=0.5 spring=0.414 damp=0.313 mass=0.1 beltmass=1
friction :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
friction rate in_ friction_ spring_ damp mass beltmass = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Friction" [in_,friction_,spring_,damp,mass,beltmass] Nothing 1 (Special 0) NoId

-- | Single gammatone filter
--
--  Gammatone [AudioRate] input=0 centrefrequency=440 bandwidth=200;    FILTER: TRUE
gammatone :: Ugen -> Ugen -> Ugen -> Ugen
gammatone input centrefrequency bandwidth = mkUgen Nothing [AudioRate] (Right [0]) "Gammatone" [input,centrefrequency,bandwidth] Nothing 1 (Special 0) NoId

-- | Gaussian classifier
--
--  GaussClass [ControlRate] in=0 bufnum=0 gate=0
gaussClass :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
gaussClass rate in_ bufnum gate_ = mkUgen Nothing [ControlRate] (Left rate) "GaussClass" [in_,bufnum,gate_] Nothing 1 (Special 0) NoId

-- | impulses around a certain frequency
--
--  GaussTrig [ControlRate,AudioRate] freq=440 dev=0.3
gaussTrig :: Rate -> Ugen -> Ugen -> Ugen
gaussTrig rate freq dev = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "GaussTrig" [freq,dev] Nothing 1 (Special 0) NoId

-- | gingerbreadman map 2D chaotic generator
--
--  Gbman2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 x0=1.2 y0=2.1
gbman2DC :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
gbman2DC rate minfreq maxfreq x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Gbman2DC" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | gingerbreadman map 2D chaotic generator
--
--  Gbman2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 x0=1.2 y0=2.1
gbman2DL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
gbman2DL rate minfreq maxfreq x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Gbman2DL" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | gingerbreadman map 2D chaotic generator
--
--  Gbman2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 x0=1.2 y0=2.1
gbman2DN :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
gbman2DN rate minfreq maxfreq x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Gbman2DN" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GbmanTrig [ControlRate,AudioRate] minfreq=5 maxfreq=10 x0=1.2 y0=2.1
gbmanTrig :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
gbmanTrig rate minfreq maxfreq x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "GbmanTrig" [minfreq,maxfreq,x0,y0] Nothing 1 (Special 0) NoId

-- | Dynamic stochastic synthesis generator
--
--  Gendy4 [ControlRate,AudioRate] ampdist=1 durdist=1 adparam=1 ddparam=1 minfreq=440 maxfreq=660 ampscale=0.5 durscale=0.5 initCPs=12 knum=0
gendy4 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
gendy4 rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Gendy4" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) NoId

-- | Dynamic stochastic synthesis generator
--
--  Gendy5 [ControlRate,AudioRate] ampdist=1 durdist=1 adparam=1 ddparam=1 minfreq=440 maxfreq=660 ampscale=0.5 durscale=0.5 initCPs=12 knum=0
gendy5 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
gendy5 rate ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Gendy5" [ampdist,durdist,adparam,ddparam,minfreq,maxfreq,ampscale,durscale,initCPs,knum] Nothing 1 (Special 0) NoId

-- | Read (numeric) shell environment variables into a synth
--
--  Getenv [] key=0 defaultval=0
getenv :: Rate -> Ugen -> Ugen -> Ugen
getenv rate key defaultval = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "Getenv" [key,defaultval] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchBPF [ControlRate,AudioRate] in=0 freq=440 rq=1
glitchBPF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
glitchBPF rate in_ freq rq = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "GlitchBPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchBRF [ControlRate,AudioRate] in=0 freq=440 rq=1
glitchBRF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
glitchBRF rate in_ freq rq = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "GlitchBRF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchHPF [ControlRate,AudioRate] in=0 freq=440
glitchHPF :: Rate -> Ugen -> Ugen -> Ugen
glitchHPF rate in_ freq = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "GlitchHPF" [in_,freq] Nothing 1 (Special 0) NoId

-- | backward compatibility
--
--  GlitchRHPF [ControlRate,AudioRate] in=0 freq=440 rq=1;    FILTER: TRUE
glitchRHPF :: Ugen -> Ugen -> Ugen -> Ugen
glitchRHPF in_ freq rq = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "GlitchRHPF" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | Calculate a single DFT bin, to detect presence of a frequency
--
--  Goertzel [ControlRate] in=0 bufsize=1024 freq=0 hop=1
goertzel :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
goertzel rate in_ bufsize freq hop = mkUgen Nothing [ControlRate] (Left rate) "Goertzel" [in_,bufsize,freq,hop] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainBufJ [AudioRate] numChannels=1 trigger=0 dur=1 sndbuf=0 rate=1 pos=0 loop=0 interp=2 grainAmp=1 pan=0 envbufnum=-1 maxGrains=512
grainBufJ :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
grainBufJ rate numChannels trigger dur sndbuf rate_ pos loop interp grainAmp pan envbufnum maxGrains = mkUgen Nothing [AudioRate] (Left rate) "GrainBufJ" [numChannels,trigger,dur,sndbuf,rate_,pos,loop,interp,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainFMJ [AudioRate] numChannels=1 trigger=0 dur=1 carfreq=440 modfreq=200 index=1 grainAmp=1 pan=0 envbufnum=-1 maxGrains=512
grainFMJ :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
grainFMJ rate numChannels trigger dur carfreq modfreq index_ grainAmp pan envbufnum maxGrains = mkUgen Nothing [AudioRate] (Left rate) "GrainFMJ" [numChannels,trigger,dur,carfreq,modfreq,index_,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainInJ [AudioRate] numChannels=1 trigger=0 dur=1 in=0 grainAmp=1 pan=0 envbufnum=-1 maxGrains=512
grainInJ :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
grainInJ rate numChannels trigger dur in_ grainAmp pan envbufnum maxGrains = mkUgen Nothing [AudioRate] (Left rate) "GrainInJ" [numChannels,trigger,dur,in_,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  GrainSinJ [AudioRate] numChannels=1 trigger=0 dur=1 freq=440 grainAmp=1 pan=0 envbufnum=-1 maxGrains=512
grainSinJ :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
grainSinJ rate numChannels trigger dur freq grainAmp pan envbufnum maxGrains = mkUgen Nothing [AudioRate] (Left rate) "GrainSinJ" [numChannels,trigger,dur,freq,grainAmp,pan,envbufnum,maxGrains] Nothing 1 (Special 0) NoId

-- | dynamical system simulation (Newtonian gravitational force)
--
--  GravityGrid [AudioRate] reset=0 rate=0.1 newx=0 newy=0 bufnum=0
gravityGrid :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
gravityGrid rate reset rate_ newx newy bufnum = mkUgen Nothing [AudioRate] (Left rate) "GravityGrid" [reset,rate_,newx,newy,bufnum] Nothing 1 (Special 0) NoId

-- | dynamical system simulation (Newtonian gravitational force)
--
--  GravityGrid2 [AudioRate] reset=0 rate=0.1 newx=0 newy=0 bufnum=0
gravityGrid2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
gravityGrid2 rate reset rate_ newx newy bufnum = mkUgen Nothing [AudioRate] (Left rate) "GravityGrid2" [reset,rate_,newx,newy,bufnum] Nothing 1 (Special 0) NoId

-- | algorithmic delay
--
--  GreyholeRaw [AudioRate] in1=0 in2=0 damping=0 delaytime=2 diffusion=0.5 feedback=0.9 moddepth=0.1 modfreq=2 size=1;    FILTER: TRUE
greyholeRaw :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
greyholeRaw in1 in2 damping delaytime diffusion feedback moddepth modfreq size = mkUgen Nothing [AudioRate] (Right [0,1]) "GreyholeRaw" [in1,in2,damping,delaytime,diffusion,feedback,moddepth,modfreq,size] Nothing 2 (Special 0) NoId

-- | Simple cochlear hair cell model
--
--  HairCell [ControlRate,AudioRate] input=0 spontaneousrate=0 boostrate=200 restorerate=1000 loss=0.99;    FILTER: TRUE
hairCell :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
hairCell input spontaneousrate boostrate restorerate loss = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "HairCell" [input,spontaneousrate,boostrate,restorerate,loss] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  HarmonicOsc [ControlRate,AudioRate] freq=100 firstharmonic=1 *amplitudes=0;    MCE=1
harmonicOsc :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
harmonicOsc rate freq firstharmonic amplitudes = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "HarmonicOsc" [freq,firstharmonic] (Just [amplitudes]) 1 (Special 0) NoId

-- | henon map 2D chaotic generator
--
--  Henon2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1.4 b=0.3 x0=0.30502 y0=0.20939
henon2DC :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
henon2DC rate minfreq maxfreq a b x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Henon2DC" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | henon map 2D chaotic generator
--
--  Henon2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1.4 b=0.3 x0=0.30502 y0=0.20939
henon2DL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
henon2DL rate minfreq maxfreq a b x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Henon2DL" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | henon map 2D chaotic generator
--
--  Henon2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1.4 b=0.3 x0=0.30502 y0=0.20939
henon2DN :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
henon2DN rate minfreq maxfreq a b x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Henon2DN" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  HenonTrig [ControlRate,AudioRate] minfreq=5 maxfreq=10 a=1.4 b=0.3 x0=0.30502 y0=0.20939
henonTrig :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
henonTrig rate minfreq maxfreq a b x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "HenonTrig" [minfreq,maxfreq,a,b,x0,y0] Nothing 1 (Special 0) NoId

-- | Transform a cepstrum back to a spectrum
--
--  ICepstrum [] cepchain=0 fftbuf=0
iCepstrum :: Rate -> Ugen -> Ugen -> Ugen
iCepstrum rate cepchain fftbuf = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "ICepstrum" [cepchain,fftbuf] Nothing 1 (Special 0) NoId

-- | 24db/oct rolloff, 4nd order resonant Low Pass Filter
--
--  IirFilter [AudioRate] in=0 freq=440 rq=1;    FILTER: TRUE
iirFilter :: Ugen -> Ugen -> Ugen -> Ugen
iirFilter in_ freq rq = mkUgen Nothing [AudioRate] (Right [0]) "IirFilter" [in_,freq,rq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrain [AudioRate] trigger=0 dur=1 in=0
inGrain :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
inGrain rate trigger dur in_ = mkUgen Nothing [AudioRate] (Left rate) "InGrain" [trigger,dur,in_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainB [AudioRate] trigger=0 dur=1 in=0 envbuf=0
inGrainB :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
inGrainB rate trigger dur in_ envbuf = mkUgen Nothing [AudioRate] (Left rate) "InGrainB" [trigger,dur,in_,envbuf] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainBBF [AudioRate] trigger=0 dur=1 in=0 envbuf=0 azimuth=0 elevation=0 rho=1 wComp=0
inGrainBBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
inGrainBBF rate trigger dur in_ envbuf azimuth elevation rho wComp = mkUgen Nothing [AudioRate] (Left rate) "InGrainBBF" [trigger,dur,in_,envbuf,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainBF [AudioRate] trigger=0 dur=1 in=0 azimuth=0 elevation=0 rho=1 wComp=0
inGrainBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
inGrainBF rate trigger dur in_ azimuth elevation rho wComp = mkUgen Nothing [AudioRate] (Left rate) "InGrainBF" [trigger,dur,in_,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainI [AudioRate] trigger=0 dur=1 in=0 envbuf1=0 envbuf2=0 ifac=0.5
inGrainI :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
inGrainI rate trigger dur in_ envbuf1 envbuf2 ifac = mkUgen Nothing [AudioRate] (Left rate) "InGrainI" [trigger,dur,in_,envbuf1,envbuf2,ifac] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  InGrainIBF [AudioRate] trigger=0 dur=1 in=0 envbuf1=0 envbuf2=0 ifac=0.5 azimuth=0 elevation=0 rho=1 wComp=0
inGrainIBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
inGrainIBF rate trigger dur in_ envbuf1 envbuf2 ifac azimuth elevation rho wComp = mkUgen Nothing [AudioRate] (Left rate) "InGrainIBF" [trigger,dur,in_,envbuf1,envbuf2,ifac,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Distortion by subtracting magnitude from 1
--
--  InsideOut [ControlRate,AudioRate] in=0
insideOut :: Rate -> Ugen -> Ugen
insideOut rate in_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "InsideOut" [in_] Nothing 1 (Special 0) NoId

-- | instruction synthesis (breakpoint set interpreter)
--
--  Instruction [AudioRate] bufnum=0
instruction :: Rate -> Ugen -> Ugen
instruction rate bufnum = mkUgen Nothing [AudioRate] (Left rate) "Instruction" [bufnum] Nothing 1 (Special 0) NoId

-- | Raw version of the JPverb algorithmic reverberator, designed to produce long tails with chorusing
--
--  JPverbRaw [ControlRate,AudioRate] in1=0 in2=0 damp=0 earlydiff=0.707 highband=2000 highx=1 lowband=500 lowx=1 mdepth=0.1 mfreq=2 midx=1 size=1 t60=1;    FILTER: TRUE
jPverbRaw :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
jPverbRaw in1 in2 damp earlydiff highband highx lowband lowx mdepth mfreq midx size t60 = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "JPverbRaw" [in1,in2,damp,earlydiff,highband,highx,lowband,lowx,mdepth,mfreq,midx,size,t60] Nothing 2 (Special 0) NoId

-- | k-means classification in real time
--
--  KMeansRT [ControlRate] bufnum=0 inputdata=0 k=5 gate=1 reset=0 learn=1
kMeansRT :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
kMeansRT rate bufnum inputdata k gate_ reset learn = mkUgen Nothing [ControlRate] (Left rate) "KMeansRT" [bufnum,inputdata,k,gate_,reset,learn] Nothing 1 (Special 0) NoId

-- | Running score of maximum correlation of chromagram with key profiles
--
--  KeyClarity [ControlRate] chain=0 keydecay=2 chromaleak=0.5
keyClarity :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
keyClarity rate chain keydecay chromaleak = mkUgen Nothing [ControlRate] (Left rate) "KeyClarity" [chain,keydecay,chromaleak] Nothing 1 (Special 0) NoId

-- | Find best correlated key mode with chromagram between major, minor and chromatic cluster
--
--  KeyMode [ControlRate] chain=0 keydecay=2 chromaleak=0.5
keyMode :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
keyMode rate chain keydecay chromaleak = mkUgen Nothing [ControlRate] (Left rate) "KeyMode" [chain,keydecay,chromaleak] Nothing 1 (Special 0) NoId

-- | K-means Oscillator
--
--  KmeansToBPSet1 [AudioRate] freq=440 numdatapoints=20 maxnummeans=4 nummeans=4 tnewdata=1 tnewmeans=1 soft=1 bufnum=0
kmeansToBPSet1 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
kmeansToBPSet1 rate freq numdatapoints maxnummeans nummeans tnewdata tnewmeans soft bufnum = mkUgen Nothing [AudioRate] (Left rate) "KmeansToBPSet1" [freq,numdatapoints,maxnummeans,nummeans,tnewdata,tnewmeans,soft,bufnum] Nothing 1 (Special 0) NoId

{-
-- | Run any LADSPA plugin inside SuperCollider
--
--  LADSPA [AudioRate] nChans=0 id=0 args=0
ladspa :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
ladspa rate nChans id_ args = mkUgen Nothing [AudioRate] (Left rate) "LADSPA" [nChans,id_,args] Nothing 0 (Special 0) NoId
-}

-- | random walk step
--
--  LFBrownNoise0 [ControlRate,AudioRate] freq=20 dev=1 dist=0;    NONDET
lfBrownNoise0Id :: ID a => a -> Rate -> Ugen -> Ugen -> Ugen -> Ugen
lfBrownNoise0Id z rate freq dev dist = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "LFBrownNoise0" [freq,dev,dist] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of LFBrownNoise0.
lfBrownNoise0M :: Uid m => Rate -> Ugen -> Ugen -> Ugen -> m Ugen
lfBrownNoise0M = liftUid4 lfBrownNoise0Id

-- | Unsafe variant of LFBrownNoise0.
lfBrownNoise0 ::  Rate -> Ugen -> Ugen -> Ugen -> Ugen
lfBrownNoise0 = liftUnsafe4 lfBrownNoise0M

-- | random walk linear interp
--
--  LFBrownNoise1 [ControlRate,AudioRate] freq=20 dev=1 dist=0;    NONDET
lfBrownNoise1Id :: ID a => a -> Rate -> Ugen -> Ugen -> Ugen -> Ugen
lfBrownNoise1Id z rate freq dev dist = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "LFBrownNoise1" [freq,dev,dist] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of LFBrownNoise1.
lfBrownNoise1M :: Uid m => Rate -> Ugen -> Ugen -> Ugen -> m Ugen
lfBrownNoise1M = liftUid4 lfBrownNoise1Id

-- | Unsafe variant of LFBrownNoise1.
lfBrownNoise1 ::  Rate -> Ugen -> Ugen -> Ugen -> Ugen
lfBrownNoise1 = liftUnsafe4 lfBrownNoise1M

-- | random walk cubic interp
--
--  LFBrownNoise2 [ControlRate,AudioRate] freq=20 dev=1 dist=0;    NONDET
lfBrownNoise2Id :: ID a => a -> Rate -> Ugen -> Ugen -> Ugen -> Ugen
lfBrownNoise2Id z rate freq dev dist = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "LFBrownNoise2" [freq,dev,dist] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of LFBrownNoise2.
lfBrownNoise2M :: Uid m => Rate -> Ugen -> Ugen -> Ugen -> m Ugen
lfBrownNoise2M = liftUid4 lfBrownNoise2Id

-- | Unsafe variant of LFBrownNoise2.
lfBrownNoise2 ::  Rate -> Ugen -> Ugen -> Ugen -> Ugen
lfBrownNoise2 = liftUnsafe4 lfBrownNoise2M

-- | Live Linear Predictive Coding Analysis and Resynthesis
--
--  LPCAnalyzer [AudioRate] input=0 source=0.01 n=256 p=10 testE=0 delta=0.999 windowtype=0;    FILTER: TRUE
lpcAnalyzer :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
lpcAnalyzer input source n p testE delta windowtype = mkUgen Nothing [AudioRate] (Right [0,1]) "LPCAnalyzer" [input,source,n,p,testE,delta,windowtype] Nothing 1 (Special 0) NoId

-- | Linear Predictive Coding Gone Wrong
--
--  LPCError [AudioRate] input=0 p=10
lpcError :: Rate -> Ugen -> Ugen -> Ugen
lpcError rate input p = mkUgen Nothing [AudioRate] (Left rate) "LPCError" [input,p] Nothing 1 (Special 0) NoId

-- | Utilize LPC data
--
--  LPCSynth [AudioRate] buffer=0 signal=0 pointer=0
lpcSynth :: Ugen -> Ugen -> Ugen -> Ugen
lpcSynth buffer signal pointer = mkUgen Nothing [AudioRate] (Left AudioRate) "LPCSynth" [buffer,signal,pointer] Nothing 1 (Special 0) NoId

-- | Utilize LPC data
--
--  LPCVals [ControlRate,AudioRate] buffer=0 pointer=0
lpcVals :: Ugen -> Ugen -> Ugen
lpcVals buffer pointer = mkUgen Nothing [ControlRate,AudioRate] (Left AudioRate) "LPCVals" [buffer,pointer] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  LPF1 [ControlRate,AudioRate] in=0 freq=1000
lpf1 :: Rate -> Ugen -> Ugen -> Ugen
lpf1 rate in_ freq = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "LPF1" [in_,freq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LPF18 [AudioRate] in=0 freq=100 res=1 dist=0.4
lpf18 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
lpf18 rate in_ freq res dist = mkUgen Nothing [AudioRate] (Left rate) "LPF18" [in_,freq,res,dist] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LPFVS6 [ControlRate,AudioRate] in=0 freq=1000 slope=0.5
lpfvs6 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
lpfvs6 rate in_ freq slope_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "LPFVS6" [in_,freq,slope_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LPG [AudioRate] input=0 controlinput=0 controloffset=0 controlscale=1 vca=1 resonance=1.5 lowpassmode=1 linearity=1;    FILTER: TRUE
lpg :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
lpg input controlinput controloffset controlscale vca resonance lowpassmode linearity = mkUgen Nothing [AudioRate] (Right [0]) "LPG" [input,controlinput,controloffset,controlscale,vca,resonance,lowpassmode,linearity] Nothing 1 (Special 0) NoId

-- | Linear Time Invariant General Filter Equation
--
--  LTI [AudioRate] input=0 bufnuma=0 bufnumb=1
lti :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
lti rate input bufnuma bufnumb = mkUgen Nothing [AudioRate] (Left rate) "LTI" [input,bufnuma,bufnumb] Nothing 1 (Special 0) NoId

-- | latoocarfian 2D chaotic generator
--
--  Latoocarfian2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1 b=3 c=0.5 d=0.5 x0=0.34082 y0=-0.3827
latoocarfian2DC :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
latoocarfian2DC rate minfreq maxfreq a b c d x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Latoocarfian2DC" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | latoocarfian 2D chaotic generator
--
--  Latoocarfian2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1 b=3 c=0.5 d=0.5 x0=0.34082 y0=-0.3827
latoocarfian2DL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
latoocarfian2DL rate minfreq maxfreq a b c d x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Latoocarfian2DL" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | latoocarfian 2D chaotic generator
--
--  Latoocarfian2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 a=1 b=3 c=0.5 d=0.5 x0=0.34082 y0=-0.3827
latoocarfian2DN :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
latoocarfian2DN rate minfreq maxfreq a b c d x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Latoocarfian2DN" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LatoocarfianTrig [ControlRate,AudioRate] minfreq=5 maxfreq=10 a=1 b=3 c=0.5 d=0.5 x0=0.34082 y0=-0.3827
latoocarfianTrig :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
latoocarfianTrig rate minfreq maxfreq a b c d x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "LatoocarfianTrig" [minfreq,maxfreq,a,b,c,d,x0,y0] Nothing 1 (Special 0) NoId

-- | Emit a sequence of triggers at specified time offsets
--
--  ListTrig [ControlRate] bufnum=0 reset=0 offset=0 numframes=0
listTrig :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
listTrig rate bufnum reset offset numframes = mkUgen Nothing [ControlRate] (Left rate) "ListTrig" [bufnum,reset,offset,numframes] Nothing 1 (Special 0) NoId

-- | Emit a sequence of triggers at specified time offsets
--
--  ListTrig2 [ControlRate] bufnum=0 reset=0 numframes=0
listTrig2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
listTrig2 rate bufnum reset numframes = mkUgen Nothing [ControlRate] (Left rate) "ListTrig2" [bufnum,reset,numframes] Nothing 1 (Special 0) NoId

-- | Store values to a buffer, whenever triggered
--
--  Logger [ControlRate] inputArray=0 trig=0 bufnum=0 reset=0
logger :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
logger rate inputArray trig_ bufnum reset = mkUgen Nothing [ControlRate] (Left rate) "Logger" [inputArray,trig_,bufnum,reset] Nothing 1 (Special 0) NoId

-- | sample looping oscillator
--
--  LoopBuf [AudioRate] bufnum=0 rate=1 gate=1 startPos=0 startLoop=0 endLoop=0 interpolation=2;    NC INPUT: True
loopBuf :: Int -> Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
loopBuf numChannels rate bufnum rate_ gate_ startPos startLoop endLoop interpolation = mkUgen Nothing [AudioRate] (Left rate) "LoopBuf" [bufnum,rate_,gate_,startPos,startLoop,endLoop,interpolation] Nothing numChannels (Special 0) NoId

-- | lorenz 2D chaotic generator
--
--  Lorenz2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 s=10 r=28 b=2.66667 h=0.02 x0=0.09088 y0=2.97077 z0=24.28204
lorenz2DC :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
lorenz2DC rate minfreq maxfreq s r b h x0 y0 z0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Lorenz2DC" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | lorenz 2D chaotic generator
--
--  Lorenz2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 s=10 r=28 b=2.66667 h=0.02 x0=0.09088 y0=2.97077 z0=24.28204
lorenz2DL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
lorenz2DL rate minfreq maxfreq s r b h x0 y0 z0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Lorenz2DL" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | lorenz 2D chaotic generator
--
--  Lorenz2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 s=10 r=28 b=2.66667 h=0.02 x0=0.09088 y0=2.97077 z0=24.28204
lorenz2DN :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
lorenz2DN rate minfreq maxfreq s r b h x0 y0 z0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Lorenz2DN" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | lorenz chaotic trigger generator
--
--  LorenzTrig [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 s=10 r=28 b=2.66667 h=0.02 x0=0.09088 y0=2.97077 z0=24.28204
lorenzTrig :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
lorenzTrig rate minfreq maxfreq s r b h x0 y0 z0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "LorenzTrig" [minfreq,maxfreq,s,r,b,h,x0,y0,z0] Nothing 1 (Special 0) NoId

-- | simple resonating lowpass filter
--
--  Lores [AudioRate] in=0 freq=880 res=0.5;    FILTER: TRUE
lores :: Ugen -> Ugen -> Ugen -> Ugen
lores in_ freq res = mkUgen Nothing [AudioRate] (Right [0]) "Lores" [in_,freq,res] Nothing 1 (Special 0) NoId

-- | 2-species Predator-Prey model
--
--  LotkaVolterra [AudioRate] freq=22050 a=1.5 b=1.5 c=0.5 d=1.5 h=0.05 xi=1 yi=0.2
lotkaVolterra :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
lotkaVolterra rate freq a b c d h xi yi = mkUgen Nothing [AudioRate] (Left rate) "LotkaVolterra" [freq,a,b,c,d,h,xi,yi] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  MCLDChaosGen [] maxSize=0
mcldChaosGen :: Rate -> Ugen -> Ugen
mcldChaosGen rate maxSize = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "MCLDChaosGen" [maxSize] Nothing 1 (Special 0) NoId

-- | POKEY Chip Sound Simulator
--
--  MZPokey [AudioRate] audf1=0 audc1=0 audf2=0 audc2=0 audf3=0 audc3=0 audf4=0 audc4=0 audctl=0
mzPokey :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
mzPokey rate audf1 audc1 audf2 audc2 audf3 audc3 audf4 audc4 audctl = mkUgen Nothing [AudioRate] (Left rate) "MZPokey" [audf1,audc1,audf2,audc2,audf3,audc3,audf4,audc4,audctl] Nothing 1 (Special 0) NoId

-- | First order Markov Chain implementation for audio signals
--
--  MarkovSynth [AudioRate] in=0 isRecording=1 waitTime=2 tableSize=10
markovSynth :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
markovSynth rate in_ isRecording waitTime tableSize = mkUgen Nothing [AudioRate] (Left rate) "MarkovSynth" [in_,isRecording,waitTime,tableSize] Nothing 1 (Special 0) NoId

-- | Real time sparse representation
--
--  MatchingP [ControlRate,AudioRate] dict=0 in=0 dictsize=1 ntofind=1 hop=1 method=0
matchingP :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
matchingP rate dict in_ dictsize ntofind hop method = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "MatchingP" [dict,in_,dictsize,ntofind,hop,method] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  MatchingPResynth [ControlRate,AudioRate] dict=0 method=0 trigger=0 residual=0 activs=0
matchingPResynth :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
matchingPResynth rate dict method trigger residual activs = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "MatchingPResynth" [dict,method,trigger,residual,activs] Nothing 1 (Special 0) NoId

-- | maximum within last x samples
--
--  Max [ControlRate] in=0 numsamp=64
max :: Rate -> Ugen -> Ugen -> Ugen
max rate in_ numsamp = mkUgen Nothing [ControlRate] (Left rate) "Max" [in_,numsamp] Nothing 1 (Special 0) NoId

-- | Tracks and prints amplitudes
--
--  Maxamp [AudioRate] in=0 numSamps=1000
maxamp :: Rate -> Ugen -> Ugen -> Ugen
maxamp rate in_ numSamps = mkUgen Nothing [AudioRate] (Left rate) "Maxamp" [in_,numSamps] Nothing 1 (Special 0) NoId

-- | Piano synthesiser
--
--  MdaPiano [AudioRate] freq=440 gate=1 vel=100 decay=0.8 release=0.8 hard=0.8 velhard=0.8 muffle=0.8 velmuff=0.8 velcurve=0.8 stereo=0.2 tune=0.5 random=0.1 stretch=0.1 sustain=0
mdaPiano :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
mdaPiano rate freq gate_ vel decay_ release hard velhard muffle velmuff velcurve stereo tune random stretch sustain = mkUgen Nothing [AudioRate] (Left rate) "MdaPiano" [freq,gate_,vel,decay_,release,hard,velhard,muffle,velmuff,velcurve,stereo,tune,random,stretch,sustain] Nothing 2 (Special 0) NoId

-- | Mean of recent values, triggered
--
--  MeanTriggered [ControlRate,AudioRate] in=0 trig=0 length=10
meanTriggered :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
meanTriggered rate in_ trig_ length_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "MeanTriggered" [in_,trig_,length_] Nothing 1 (Special 0) NoId

-- | Meddis cochlear hair cell model
--
--  Meddis [ControlRate,AudioRate] input=0;    FILTER: TRUE
meddis :: Ugen -> Ugen
meddis input = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "Meddis" [input] Nothing 1 (Special 0) NoId

-- | Separate harmonic and percussive parts of a signal
--
--  MedianSeparation [] fft=0 fftharmonic=0 fftpercussive=0 fftsize=1024 mediansize=17 hardorsoft=0 p=2 medianormax=0
medianSeparation :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
medianSeparation rate fft_ fftharmonic fftpercussive fftsize mediansize hardorsoft p medianormax = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "MedianSeparation" [fft_,fftharmonic,fftpercussive,fftsize,mediansize,hardorsoft,p,medianormax] Nothing 2 (Special 0) NoId

-- | Median of recent values, triggered
--
--  MedianTriggered [ControlRate,AudioRate] in=0 trig=0 length=10
medianTriggered :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
medianTriggered rate in_ trig_ length_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "MedianTriggered" [in_,trig_,length_] Nothing 1 (Special 0) NoId

-- | Waveguide mesh physical models of drum membranes
--
--  MembraneCircle [AudioRate] excitation=0 tension=0.05 loss=0.99999
membraneCircle :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
membraneCircle rate excitation tension loss = mkUgen Nothing [AudioRate] (Left rate) "MembraneCircle" [excitation,tension,loss] Nothing 1 (Special 0) NoId

-- | Waveguide mesh physical models of drum membranes
--
--  MembraneHexagon [AudioRate] excitation=0 tension=0.05 loss=0.99999
membraneHexagon :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
membraneHexagon rate excitation tension loss = mkUgen Nothing [AudioRate] (Left rate) "MembraneHexagon" [excitation,tension,loss] Nothing 1 (Special 0) NoId

-- | Metronome
--
--  Metro [ControlRate,AudioRate] bpm=0 numBeats=0
metro :: Rate -> Ugen -> Ugen -> Ugen
metro rate bpm numBeats = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Metro" [bpm,numBeats] Nothing 1 (Special 0) NoId

-- | a macro oscillator
--
--  MiBraids [AudioRate] pitch=60 timbre=0.5 color=0.5 model=0 trig=0 resamp=0 decim=0 bits=0 ws=0
miBraids :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miBraids rate pitch_ timbre color model trig_ resamp decim bits ws = mkUgen Nothing [AudioRate] (Left rate) "MiBraids" [pitch_,timbre,color,model,trig_,resamp,decim,bits,ws] Nothing 1 (Special 0) NoId

-- | granular audio processor and texture synthesizer
--
--  MiClouds [AudioRate] pit=0 pos=0.5 size=0.25 dens=0.4 tex=0.5 drywet=0.5 in_gain=1 spread=0.5 rvb=0 fb=0 freeze=0 mode=0 lofi=0 trig=0 *inputArray=0;    MCE=1, REORDERS INPUTS: [14,0,1,2,3,4,5,6,7,8,9,10,11,12,13]
miClouds :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miClouds rate pit pos size dens tex drywet in_gain spread rvb fb freeze mode lofi trig_ inputArray = mkUgen Nothing [AudioRate] (Left rate) "MiClouds" [pit,pos,size,dens,tex,drywet,in_gain,spread,rvb,fb,freeze,mode,lofi,trig_] (Just [inputArray]) 2 (Special 0) NoId

-- | Physical modelling based on Modal Synthesis.
--
--  MiElements [AudioRate] blow_in=0 strike_in=0 gate=0 pit=48 strength=0.5 contour=0.2 bow_level=0 blow_level=0 strike_level=0 flow=0.5 mallet=0.5 bow_timb=0.5 blow_timb=0.5 strike_timb=0.5 geom=0.25 bright=0.5 damp=0.7 pos=0.2 space=0.3 model=0 easteregg=0
miElements :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miElements rate blow_in strike_in gate_ pit strength contour bow_level blow_level strike_level flow mallet bow_timb blow_timb strike_timb geom bright damp pos space model easteregg = mkUgen Nothing [AudioRate] (Left rate) "MiElements" [blow_in,strike_in,gate_,pit,strength,contour,bow_level,blow_level,strike_level,flow,mallet,bow_timb,blow_timb,strike_timb,geom,bright,damp,pos,space,model,easteregg] Nothing 2 (Special 0) NoId

-- | topographic drum sequencer
--
--  MiGrids [AudioRate] bpm=120 map_x=0.5 map_y=0.5 chaos=0 bd_density=0.25 sd_density=0.25 hh_density=0.25 mode=1 swing=0 config=0
miGrids :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miGrids rate bpm map_x map_y chaos bd_density sd_density hh_density mode swing config = mkUgen Nothing [AudioRate] (Left rate) "MiGrids" [bpm,map_x,map_y,chaos,bd_density,sd_density,hh_density,mode,swing,config] Nothing 6 (Special 0) NoId

-- | µ-law audio companding
--
--  MiMu [AudioRate] in=0 gain=1 bypass=0
miMu :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
miMu rate in_ gain bypass = mkUgen Nothing [AudioRate] (Left rate) "MiMu" [in_,gain,bypass] Nothing 1 (Special 0) NoId

-- | FM Synth-Voice based on 'ominous'
--
--  MiOmi [AudioRate] audio_in=0 gate=0 pit=48 contour=0.2 detune=0.25 level1=0.5 level2=0.5 ratio1=0.5 ratio2=0.5 fm1=0 fm2=0 fb=0 xfb=0 filter_mode=0 cutoff=0.5 reson=0 strength=0.5 env=0.5 rotate=0.2 space=0.5
miOmi :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miOmi rate audio_in gate_ pit contour detune level1 level2 ratio1 ratio2 fm1 fm2 fb xfb filter_mode cutoff reson strength env rotate_ space = mkUgen Nothing [AudioRate] (Left rate) "MiOmi" [audio_in,gate_,pit,contour,detune,level1,level2,ratio1,ratio2,fm1,fm2,fb,xfb,filter_mode,cutoff,reson,strength,env,rotate_,space] Nothing 2 (Special 0) NoId

-- | a macro oscillator
--
--  MiPlaits [AudioRate] pitch=60 engine=0 harm=0.1 timbre=0.5 morph=0.5 trigger=0 level=0 fm_mod=0 timb_mod=0 morph_mod=0 decay=0.5 lpg_colour=0.5
miPlaits :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miPlaits rate pitch_ engine harm timbre morph trigger level fm_mod timb_mod morph_mod decay_ lpg_colour = mkUgen Nothing [AudioRate] (Left rate) "MiPlaits" [pitch_,engine,harm,timbre,morph,trigger,level,fm_mod,timb_mod,morph_mod,decay_,lpg_colour] Nothing 2 (Special 0) NoId

-- | a resonator
--
--  MiRings [AudioRate] in=0 trig=0 pit=60 struct=0.25 bright=0.5 damp=0.7 pos=0.25 model=0 poly=1 intern_exciter=0 easteregg=0 bypass=0
miRings :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miRings rate in_ trig_ pit struct bright damp pos model poly intern_exciter easteregg bypass = mkUgen Nothing [AudioRate] (Left rate) "MiRings" [in_,trig_,pit,struct,bright,damp,pos,model,poly,intern_exciter,easteregg,bypass] Nothing 2 (Special 0) NoId

-- | Classic resonant LP filter
--
--  MiRipples [AudioRate] in=0 cf=0.3 reson=0.2 drive=1;    FILTER: TRUE
miRipples :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miRipples in_ cf reson drive = mkUgen Nothing [AudioRate] (Right [0]) "MiRipples" [in_,cf,reson,drive] Nothing 1 (Special 0) NoId

-- | a quad LFO
--
--  MiTides [AudioRate] freq=1 shape=0.5 slope=0.5 smooth=0.5 shift=0.2 trig=0 clock=0 output_mode=3 ramp_mode=1 ratio=9 rate=1
miTides :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miTides rate freq shape slope_ smooth shift trig_ clock output_mode ramp_mode ratio rate_ = mkUgen Nothing [AudioRate] (Left rate) "MiTides" [freq,shape,slope_,smooth,shift,trig_,clock,output_mode,ramp_mode,ratio,rate_] Nothing 4 (Special 0) NoId

-- | stereo reverb
--
--  MiVerb [AudioRate] time=0.7 drywet=0.5 damp=0.5 hp=0.05 freeze=0 diff=0.625 *inputArray=0;    MCE=1, FILTER: TRUE, REORDERS INPUTS: [6,0,1,2,3,4,5]
miVerb :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miVerb time drywet damp hp freeze diff inputArray = mkUgen Nothing [AudioRate] (Right [6]) "MiVerb" [time,drywet,damp,hp,freeze,diff] (Just [inputArray]) 2 (Special 0) NoId

-- | a meta modulator
--
--  MiWarps [AudioRate] carrier=0 modulator=0 lev1=0.5 lev2=0.5 algo=0 timb=0 osc=0 freq=110 vgain=1 easteregg=0
miWarps :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
miWarps rate carrier modulator lev1 lev2 algo timb osc_ freq vgain easteregg = mkUgen Nothing [AudioRate] (Left rate) "MiWarps" [carrier,modulator,lev1,lev2,algo,timb,osc_,freq,vgain,easteregg] Nothing 2 (Special 0) NoId

-- | Granulates real-time input
--
--  MonoGrain [AudioRate] in=0 winsize=0.1 grainrate=10 winrandpct=0
monoGrain :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
monoGrain rate in_ winsize grainrate winrandpct = mkUgen Nothing [AudioRate] (Left rate) "MonoGrain" [in_,winsize,grainrate,winrandpct] Nothing 1 (Special 0) NoId

-- | Granulates real-time input with Ambisonic panning
--
--  MonoGrainBF [AudioRate] in=0 winsize=0.1 grainrate=10 winrandpct=0 azimuth=0 azrand=0 elevation=0 elrand=0 rho=1
monoGrainBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
monoGrainBF rate in_ winsize grainrate winrandpct azimuth azrand elevation elrand rho = mkUgen Nothing [AudioRate] (Left rate) "MonoGrainBF" [in_,winsize,grainrate,winrandpct,azimuth,azrand,elevation,elrand,rho] Nothing 4 (Special 0) NoId

-- | Moog Filter Emulation
--
--  MoogLadder [ControlRate,AudioRate] in=0 ffreq=440 res=0;    FILTER: TRUE
moogLadder :: Ugen -> Ugen -> Ugen -> Ugen
moogLadder in_ ffreq res = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "MoogLadder" [in_,ffreq,res] Nothing 1 (Special 0) NoId

-- | Moog  filter emulation
--
--  MoogVCF [AudioRate] in=0 fco=0 res=0;    FILTER: TRUE
moogVCF :: Ugen -> Ugen -> Ugen -> Ugen
moogVCF in_ fco res = mkUgen Nothing [AudioRate] (Right [0]) "MoogVCF" [in_,fco,res] Nothing 1 (Special 0) NoId

-- | Stereo reverb
--
--  NHHall [AudioRate] in1=0.0 in2=0.0 rt60=1.0 stereo=0.5 lowFreq=200.0 lowRatio=0.5 hiFreq=4000.0 hiRatio=0.5 earlyDiffusion=0.5 lateDiffusion=0.5 modRate=0.2 modDepth=0.3
nhHall :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nhHall in1 in2 rt60 stereo lowFreq lowRatio hiFreq hiRatio earlyDiffusion lateDiffusion modRate modDepth = mkUgen Nothing [AudioRate] (Right [0,1]) "NHHall" [in1,in2,rt60,stereo,lowFreq,lowRatio,hiFreq,hiRatio,earlyDiffusion,lateDiffusion,modRate,modDepth] Nothing 2 (Special 0) NoId

-- | Non Linear Filter Equation
--
--  NL [AudioRate] input=0 bufnuma=0 bufnumb=1 guard1=1000 guard2=100
nl :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nl rate input bufnuma bufnumb guard1 guard2 = mkUgen Nothing [AudioRate] (Left rate) "NL" [input,bufnuma,bufnumb,guard1,guard2] Nothing 1 (Special 0) NoId

-- | Arbitrary Non Linear Filter Equation
--
--  NL2 [AudioRate] input=0 bufnum=0 maxsizea=10 maxsizeb=10 guard1=1000 guard2=100
nl2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nl2 rate input bufnum maxsizea maxsizeb guard1 guard2 = mkUgen Nothing [AudioRate] (Left rate) "NL2" [input,bufnum,maxsizea,maxsizeb,guard1,guard2] Nothing 1 (Special 0) NoId

-- | Non-linear Filter
--
--  NLFiltC [ControlRate,AudioRate] input=0 a=0 b=0 d=0 c=0 l=0
nlFiltC :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nlFiltC rate input a b d c l = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "NLFiltC" [input,a,b,d,c,l] Nothing 1 (Special 0) NoId

-- | Non-linear Filter
--
--  NLFiltL [ControlRate,AudioRate] input=0 a=0 b=0 d=0 c=0 l=0
nlFiltL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nlFiltL rate input a b d c l = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "NLFiltL" [input,a,b,d,c,l] Nothing 1 (Special 0) NoId

-- | Non-linear Filter
--
--  NLFiltN [ControlRate,AudioRate] input=0 a=0 b=0 d=0 c=0 l=0
nlFiltN :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nlFiltN rate input a b d c l = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "NLFiltN" [input,a,b,d,c,l] Nothing 1 (Special 0) NoId

-- | physical modeling simulation; N tubes
--
--  NTube [AudioRate] input=0 lossarray=1 karray=0 delaylengtharray=0
nTube :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nTube rate input lossarray karray delaylengtharray = mkUgen Nothing [AudioRate] (Left rate) "NTube" [input,lossarray,karray,delaylengtharray] Nothing 1 (Special 0) NoId

-- | Find the nearest-neighbours in a set of points
--
--  NearestN [ControlRate] treebuf=0 in=0 gate=1 num=1
nearestN :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nearestN rate treebuf in_ gate_ num = mkUgen Nothing [ControlRate] (Left rate) "NearestN" [treebuf,in_,gate_,num] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  NeedleRect [AudioRate] rate=1 imgWidth=100 imgHeight=100 rectX=0 rectY=0 rectW=100 rectH=100
needleRect :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
needleRect rate rate_ imgWidth imgHeight rectX rectY rectW rectH = mkUgen Nothing [AudioRate] (Left rate) "NeedleRect" [rate_,imgWidth,imgHeight,rectX,rectY,rectW,rectH] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  NeoFormant [ControlRate,AudioRate] formantfreq=100 carrierfreq=200 phaseshift=0.5
neoFormant :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
neoFormant rate formantfreq carrierfreq phaseshift = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "NeoFormant" [formantfreq,carrierfreq,phaseshift] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  NeoVarSawOsc [ControlRate,AudioRate] freq=100 pw=0.5 waveshape=0.5
neoVarSawOsc :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
neoVarSawOsc rate freq pw waveshape = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "NeoVarSawOsc" [freq,pw,waveshape] Nothing 1 (Special 0) NoId

-- | APU Chip Sound Simulator
--
--  Nes2 [AudioRate] trig=0 a0=0 a1=0 a2=0 a3=0 b0=0 b1=0 b2=0 b3=0 c0=0 c2=0 c3=0 d0=0 d2=0 d3=0 e0=0 e1=0 e2=0 e3=0 smask=0
nes2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nes2 rate trig_ a0 a1 a2 a3 b0 b1 b2 b3 c0 c2 c3 d0 d2 d3 e0 e1 e2 e3 smask = mkUgen Nothing [AudioRate] (Left rate) "Nes2" [trig_,a0,a1,a2,a3,b0,b1,b2,b3,c0,c2,c3,d0,d2,d3,e0,e1,e2,e3,smask] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  NestedAllpassC [AudioRate] in=0 maxdelay1=0.036 delay1=0.036 gain1=0.08 maxdelay2=0.03 delay2=0.03 gain2=0.3;    FILTER: TRUE
nestedAllpassC :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nestedAllpassC in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 = mkUgen Nothing [AudioRate] (Right [0]) "NestedAllpassC" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  NestedAllpassL [AudioRate] in=0 maxdelay1=0.036 delay1=0.036 gain1=0.08 maxdelay2=0.03 delay2=0.03 gain2=0.3;    FILTER: TRUE
nestedAllpassL :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nestedAllpassL in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 = mkUgen Nothing [AudioRate] (Right [0]) "NestedAllpassL" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2] Nothing 1 (Special 0) NoId

-- | Nested Allpass filters as proposed by Vercoe and Pluckett
--
--  NestedAllpassN [AudioRate] in=0 maxdelay1=0.036 delay1=0.036 gain1=0.08 maxdelay2=0.03 delay2=0.03 gain2=0.3;    FILTER: TRUE
nestedAllpassN :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
nestedAllpassN in_ maxdelay1 delay1_ gain1 maxdelay2 delay2_ gain2 = mkUgen Nothing [AudioRate] (Right [0]) "NestedAllpassN" [in_,maxdelay1,delay1_,gain1,maxdelay2,delay2_,gain2] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSFold4 [AudioRate] in=0 lo=0 hi=0
osFold4 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
osFold4 rate in_ lo hi = mkUgen Nothing [AudioRate] (Left rate) "OSFold4" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSFold8 [AudioRate] in=0 lo=0 hi=0
osFold8 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
osFold8 rate in_ lo hi = mkUgen Nothing [AudioRate] (Left rate) "OSFold8" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSTrunc4 [AudioRate] in=0 quant=0.5
osTrunc4 :: Rate -> Ugen -> Ugen -> Ugen
osTrunc4 rate in_ quant = mkUgen Nothing [AudioRate] (Left rate) "OSTrunc4" [in_,quant] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSTrunc8 [AudioRate] in=0 quant=0.5
osTrunc8 :: Rate -> Ugen -> Ugen -> Ugen
osTrunc8 rate in_ quant = mkUgen Nothing [AudioRate] (Left rate) "OSTrunc8" [in_,quant] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSWrap4 [AudioRate] in=0 lo=0 hi=0
osWrap4 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
osWrap4 rate in_ lo hi = mkUgen Nothing [AudioRate] (Left rate) "OSWrap4" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OSWrap8 [AudioRate] in=0 lo=0 hi=0
osWrap8 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
osWrap8 rate in_ lo hi = mkUgen Nothing [AudioRate] (Left rate) "OSWrap8" [in_,lo,hi] Nothing 1 (Special 0) NoId

-- | Extract basic statistics from a series of onset triggers
--
--  OnsetStatistics [ControlRate] input=0 windowsize=1 hopsize=0.1
onsetStatistics :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
onsetStatistics rate input windowsize hopsize = mkUgen Nothing [ControlRate] (Left rate) "OnsetStatistics" [input,windowsize,hopsize] Nothing 3 (Special 0) NoId

-- | Chemical reaction modelling Oscillator
--
--  Oregonator [AudioRate] reset=0 rate=0.01 epsilon=1 mu=1 q=1 initx=0.5 inity=0.5 initz=0.5
oregonator :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
oregonator rate reset rate_ epsilon mu q initx inity initz = mkUgen Nothing [AudioRate] (Left rate) "Oregonator" [reset,rate_,epsilon,mu,q,initx,inity,initz] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  OscBank [ControlRate,AudioRate] freq=100 gain=1 saw8=0.5 square8=0.5 saw4=0.5 square4=0.5 saw2=0.5 square2=0.5 saw1=0.5
oscBank :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
oscBank rate freq gain saw8 square8 saw4 square4 saw2 square2 saw1 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "OscBank" [freq,gain,saw8,square8,saw4,square4,saw2,square2,saw1] Nothing 1 (Special 0) NoId

-- | Piano physical model.
--
--  OteyPiano [AudioRate] freq=440 vel=1 t_gate=0 rmin=0.35 rmax=2 rampl=4 rampr=8 rcore=1 lmin=0.07 lmax=1.4 lampl=-4 lampr=4 rho=1 e=1 zb=1 zh=0 mh=1 k=0.2 alpha=1 p=1 hpos=0.142 loss=1 detune=0.0003 hammer_type=1
oteyPiano :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
oteyPiano rate freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type = mkUgen Nothing [AudioRate] (Left rate) "OteyPiano" [freq,vel,t_gate,rmin,rmax,rampl,rampr,rcore,lmin,lmax,lampl,lampr,rho,e,zb,zh,mh,k,alpha,p,hpos,loss,detune,hammer_type] Nothing 1 (Special 0) NoId

-- | Piano physical model.
--
--  OteyPianoStrings [AudioRate] freq=440 vel=1 t_gate=0 rmin=0.35 rmax=2 rampl=4 rampr=8 rcore=1 lmin=0.07 lmax=1.4 lampl=-4 lampr=4 rho=1 e=1 zb=1 zh=0 mh=1 k=0.2 alpha=1 p=1 hpos=0.142 loss=1 detune=0.0003 hammer_type=1
oteyPianoStrings :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
oteyPianoStrings rate freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type = mkUgen Nothing [AudioRate] (Left rate) "OteyPianoStrings" [freq,vel,t_gate,rmin,rmax,rampl,rampr,rcore,lmin,lmax,lampl,lampr,rho,e,zb,zh,mh,k,alpha,p,hpos,loss,detune,hammer_type] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  OteySoundBoard [AudioRate] inp=0 c1=20 c3=20 mix=0.8;    FILTER: TRUE
oteySoundBoard :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
oteySoundBoard inp c1 c3 mix = mkUgen Nothing [AudioRate] (Right [0]) "OteySoundBoard" [inp,c1,c3,mix] Nothing 1 (Special 0) NoId

-- | Return mag and freq data from a CSound pv
--
--  PVInfo [ControlRate,AudioRate] pvbuffer=0 binNum=0 filePointer=0
pvInfo :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
pvInfo rate pvbuffer binNum filePointer = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "PVInfo" [pvbuffer,binNum,filePointer] Nothing 2 (Special 0) NoId

-- | Resynthesize Csound PV data
--
--  PVSynth [AudioRate] pvbuffer=0 numBins=0 binStart=0 binSkip=1 filePointer=0 freqMul=1 freqAdd=0
pvSynth :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pvSynth rate pvbuffer numBins binStart binSkip filePointer freqMul freqAdd = mkUgen Nothing [AudioRate] (Left rate) "PVSynth" [pvbuffer,numBins,binStart,binSkip,filePointer,freqMul,freqAdd] Nothing 1 (Special 0) NoId

-- | Plays FFT data to a memory buffer
--
--  PV_BinBufRd [ControlRate] buffer=0 playbuf=0 point=1 binStart=0 binSkip=1 numBins=1 clear=0
pv_BinBufRd :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_BinBufRd buffer playbuf_ point binStart binSkip numBins clear = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_BinBufRd" [buffer,playbuf_,point,binStart,binSkip,numBins,clear] Nothing 1 (Special 0) NoId

-- | Delay and Feedback on a bin by bin basis.
--
--  PV_BinDelay [ControlRate] buffer=0 maxdelay=0 delaybuf=0 fbbuf=0 hop=0.5
pv_BinDelay :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_BinDelay buffer maxdelay delaybuf fbbuf hop = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_BinDelay" [buffer,maxdelay,delaybuf,fbbuf,hop] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_BinFilter [ControlRate] buffer=0 start=0 end=0
pv_BinFilter :: Ugen -> Ugen -> Ugen -> Ugen
pv_BinFilter buffer start end = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_BinFilter" [buffer,start,end] Nothing 1 (Special 0) NoId

-- | Plays FFT data to a memory buffer
--
--  PV_BinPlayBuf [ControlRate] buffer=0 playbuf=0 rate=1 offset=0 binStart=0 binSkip=1 numBins=1 loop=0 clear=0
pv_BinPlayBuf :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_BinPlayBuf buffer playbuf_ rate_ offset binStart binSkip numBins loop clear = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_BinPlayBuf" [buffer,playbuf_,rate_,offset,binStart,binSkip,numBins,loop,clear] Nothing 1 (Special 0) NoId

-- | Plays FFT data from a memory buffer
--
--  PV_BufRd [ControlRate] buffer=0 playbuf=0 point=1
pv_BufRd :: Ugen -> Ugen -> Ugen -> Ugen
pv_BufRd buffer playbuf_ point = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_BufRd" [buffer,playbuf_,point] Nothing 1 (Special 0) NoId

-- | returns common magnitudes
--
--  PV_CommonMag [ControlRate] bufferA=0 bufferB=0 tolerance=0 remove=0
pv_CommonMag :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_CommonMag bufferA bufferB tolerance remove = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_CommonMag" [bufferA,bufferB,tolerance,remove] Nothing 1 (Special 0) NoId

-- | multiplies common magnitudes
--
--  PV_CommonMul [ControlRate] bufferA=0 bufferB=0 tolerance=0 remove=0
pv_CommonMul :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_CommonMul bufferA bufferB tolerance remove = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_CommonMul" [bufferA,bufferB,tolerance,remove] Nothing 1 (Special 0) NoId

-- | simple spectral compression/expansion
--
--  PV_Compander [ControlRate] buffer=0 thresh=50 slopeBelow=1 slopeAbove=1
pv_Compander :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_Compander buffer thresh slopeBelow slopeAbove = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_Compander" [buffer,thresh,slopeBelow,slopeAbove] Nothing 1 (Special 0) NoId

-- | zero bins with interpolation
--
--  PV_Cutoff [ControlRate] bufferA=0 bufferB=0 wipe=0
pv_Cutoff :: Ugen -> Ugen -> Ugen -> Ugen
pv_Cutoff bufferA bufferB wipe = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_Cutoff" [bufferA,bufferB,wipe] Nothing 1 (Special 0) NoId

-- | Return the even numbered bins in an FFT buffer
--
--  PV_EvenBin [ControlRate] buffer=0
pv_EvenBin :: Ugen -> Ugen
pv_EvenBin buffer = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_EvenBin" [buffer] Nothing 1 (Special 0) NoId

-- | extract a repeating loop out from audio
--
--  PV_ExtractRepeat [ControlRate] buffer=0 loopbuf=0 loopdur=0 memorytime=30 which=0 ffthop=0.5 thresh=1
pv_ExtractRepeat :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_ExtractRepeat buffer loopbuf_ loopdur memorytime which ffthop thresh = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_ExtractRepeat" [buffer,loopbuf_,loopdur,memorytime,which,ffthop,thresh] Nothing 1 (Special 0) NoId

-- | Freeze FFT frames
--
--  PV_Freeze [ControlRate] buffer=0 freeze=0
pv_Freeze :: Ugen -> Ugen -> Ugen
pv_Freeze buffer freeze = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_Freeze" [buffer,freeze] Nothing 1 (Special 0) NoId

-- | Store FFT data in another buffer for other use
--
--  PV_FreqBuffer [ControlRate] buffer=0 databuffer=0
pv_FreqBuffer :: Ugen -> Ugen -> Ugen
pv_FreqBuffer buffer databuffer = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_FreqBuffer" [buffer,databuffer] Nothing 1 (Special 0) NoId

-- | Invert FFT frames
--
--  PV_Invert [ControlRate] buffer=0
pv_Invert :: Ugen -> Ugen
pv_Invert buffer = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_Invert" [buffer] Nothing 1 (Special 0) NoId

-- | Store FFT data in another buffer for other use
--
--  PV_MagBuffer [ControlRate] buffer=0 databuffer=0
pv_MagBuffer :: Ugen -> Ugen -> Ugen
pv_MagBuffer buffer databuffer = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MagBuffer" [buffer,databuffer] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagExp [ControlRate] buffer=0
pv_MagExp :: Ugen -> Ugen
pv_MagExp buffer = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MagExp" [buffer] Nothing 1 (Special 0) NoId

-- | reduces magnitudes above or below thresh
--
--  PV_MagGate [ControlRate] buffer=0 thresh=1 remove=0
pv_MagGate :: Ugen -> Ugen -> Ugen -> Ugen
pv_MagGate buffer thresh remove = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MagGate" [buffer,thresh,remove] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagLog [ControlRate] buffer=0
pv_MagLog :: Ugen -> Ugen
pv_MagLog buffer = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MagLog" [buffer] Nothing 1 (Special 0) NoId

-- | Remap magnitudes to a new mag curve
--
--  PV_MagMap [ControlRate] buffer=0 mapbuf=0
pv_MagMap :: Ugen -> Ugen -> Ugen
pv_MagMap buffer mapbuf = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MagMap" [buffer,mapbuf] Nothing 1 (Special 0) NoId

-- | subtract spectral energy
--
--  PV_MagMinus [ControlRate] bufferA=0 bufferB=0 remove=1
pv_MagMinus :: Ugen -> Ugen -> Ugen -> Ugen
pv_MagMinus bufferA bufferB remove = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MagMinus" [bufferA,bufferB,remove] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagMulAdd [ControlRate] buffer=0
pv_MagMulAdd :: Ugen -> Ugen
pv_MagMulAdd buffer = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MagMulAdd" [buffer] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagScale [ControlRate] bufferA=0 bufferB=0
pv_MagScale :: Ugen -> Ugen -> Ugen
pv_MagScale bufferA bufferB = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MagScale" [bufferA,bufferB] Nothing 1 (Special 0) NoId

-- | Smooth spectral magnitudes over time
--
--  PV_MagSmooth [ControlRate] buffer=0 factor=0.1
pv_MagSmooth :: Ugen -> Ugen -> Ugen
pv_MagSmooth buffer factor = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MagSmooth" [buffer,factor] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_MagSubtract [ControlRate] bufferA=0 bufferB=0 zerolimit=0
pv_MagSubtract :: Ugen -> Ugen -> Ugen -> Ugen
pv_MagSubtract bufferA bufferB zerolimit = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MagSubtract" [bufferA,bufferB,zerolimit] Nothing 1 (Special 0) NoId

-- | Return the N strongest bins
--
--  PV_MaxMagN [ControlRate] buffer=0 numbins=0
pv_MaxMagN :: Ugen -> Ugen -> Ugen
pv_MaxMagN buffer numbins = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MaxMagN" [buffer,numbins] Nothing 1 (Special 0) NoId

-- | Return the N weakest bins
--
--  PV_MinMagN [ControlRate] buffer=0 numbins=0
pv_MinMagN :: Ugen -> Ugen -> Ugen
pv_MinMagN buffer numbins = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_MinMagN" [buffer,numbins] Nothing 1 (Special 0) NoId

-- | one kind of spectral morphing
--
--  PV_Morph [ControlRate] bufferA=0 bufferB=0 morph=0
pv_Morph :: Ugen -> Ugen -> Ugen -> Ugen
pv_Morph bufferA bufferB morph = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_Morph" [bufferA,bufferB,morph] Nothing 1 (Special 0) NoId

-- | Return only bins that are unstable
--
--  PV_NoiseSynthF [ControlRate] buffer=0 threshold=0.1 numFrames=2 initflag=0
pv_NoiseSynthF :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_NoiseSynthF buffer threshold numFrames initflag = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_NoiseSynthF" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | Return only bins that are unstable
--
--  PV_NoiseSynthP [ControlRate] buffer=0 threshold=0.1 numFrames=2 initflag=0
pv_NoiseSynthP :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_NoiseSynthP buffer threshold numFrames initflag = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_NoiseSynthP" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | Return the odd numbered bins in an FFT buffer
--
--  PV_OddBin [ControlRate] buffer=0
pv_OddBin :: Ugen -> Ugen
pv_OddBin buffer = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_OddBin" [buffer] Nothing 1 (Special 0) NoId

-- | Return only bins that are stable
--
--  PV_PartialSynthF [ControlRate] buffer=0 threshold=0.1 numFrames=2 initflag=0
pv_PartialSynthF :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_PartialSynthF buffer threshold numFrames initflag = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_PartialSynthF" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | Return only bins that are stable
--
--  PV_PartialSynthP [ControlRate] buffer=0 threshold=0.1 numFrames=2 initflag=0
pv_PartialSynthP :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_PartialSynthP buffer threshold numFrames initflag = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_PartialSynthP" [buffer,threshold,numFrames,initflag] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_PitchShift [ControlRate] buffer=0 ratio=0
pv_PitchShift :: Ugen -> Ugen -> Ugen
pv_PitchShift buffer ratio = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_PitchShift" [buffer,ratio] Nothing 1 (Special 0) NoId

-- | Plays FFT data to a memory buffer
--
--  PV_PlayBuf [ControlRate] buffer=0 playbuf=0 rate=1 offset=0 loop=0
pv_PlayBuf :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_PlayBuf buffer playbuf_ rate_ offset loop = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_PlayBuf" [buffer,playbuf_,rate_,offset,loop] Nothing 1 (Special 0) NoId

-- | Records FFT data to a memory buffer
--
--  PV_RecordBuf [ControlRate] buffer=0 recbuf=0 offset=0 run=0 loop=0 hop=0.5 wintype=0
pv_RecordBuf :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_RecordBuf buffer recbuf offset run loop hop wintype = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_RecordBuf" [buffer,recbuf,offset,run,loop,hop,wintype] Nothing 1 (Special 0) NoId

-- | combine low and high bins from two inputs with interpolation
--
--  PV_SoftWipe [ControlRate] bufferA=0 bufferB=0 wipe=0
pv_SoftWipe :: Ugen -> Ugen -> Ugen -> Ugen
pv_SoftWipe bufferA bufferB wipe = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_SoftWipe" [bufferA,bufferB,wipe] Nothing 1 (Special 0) NoId

-- | A harmonic enhancer
--
--  PV_SpectralEnhance [ControlRate] buffer=0 numPartials=8 ratio=2 strength=0.1
pv_SpectralEnhance :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_SpectralEnhance buffer numPartials ratio strength = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_SpectralEnhance" [buffer,numPartials,ratio,strength] Nothing 1 (Special 0) NoId

-- | Maps the spectral envelope of one FFT process onto another
--
--  PV_SpectralMap [ControlRate] buffer=0 specBuffer=0 floor=0 freeze=0 mode=0 norm=0 window=0
pv_SpectralMap :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_SpectralMap buffer specBuffer floor_ freeze mode norm window = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_SpectralMap" [buffer,specBuffer,floor_,freeze,mode,norm,window] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_Split [ControlRate] bufferA=0 bufferB=0
pv_Split :: Ugen -> Ugen -> Ugen
pv_Split bufferA bufferB = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_Split" [bufferA,bufferB] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  PV_Whiten [ControlRate] chain=0 trackbufnum=0 relaxtime=2 floor=0.1 smear=0 bindownsample=0
pv_Whiten :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_Whiten chain trackbufnum relaxtime floor_ smear bindownsample = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_Whiten" [chain,trackbufnum,relaxtime,floor_,smear,bindownsample] Nothing 1 (Special 0) NoId

-- | one kind of spectral morphing
--
--  PV_XFade [ControlRate] bufferA=0 bufferB=0 fade=0
pv_xFade :: Ugen -> Ugen -> Ugen -> Ugen
pv_xFade bufferA bufferB fade = mkUgen Nothing [ControlRate] (Left ControlRate) "PV_XFade" [bufferA,bufferB,fade] Nothing 1 (Special 0) NoId

-- | Equal power pan across an array of speakers
--
--  PanX [ControlRate,AudioRate] numChans=0 in=0 pos=0 level=1 width=2
panX :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
panX rate numChans in_ pos level width = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "PanX" [numChans,in_,pos,level,width] Nothing 0 (Special 0) NoId

-- | (Undocumented class)
--
--  PanX2D [ControlRate,AudioRate] numChansX=0 numChansY=0 in=0 posX=0 posY=0 level=1 widthX=2 widthY=2
panX2D :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
panX2D rate numChansX numChansY in_ posX posY level widthX widthY = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "PanX2D" [numChansX,numChansY,in_,posX,posY,level,widthX,widthY] Nothing 0 (Special 0) NoId

-- | (Undocumented class)
--
--  PeakEQ2 [AudioRate] in=0 freq=1200 rs=1 db=0
peakEQ2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
peakEQ2 rate in_ freq rs db = mkUgen Nothing [AudioRate] (Left rate) "PeakEQ2" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PeakEQ4 [AudioRate] in=0 freq=1200 rs=1 db=0
peakEQ4 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
peakEQ4 rate in_ freq rs db = mkUgen Nothing [AudioRate] (Left rate) "PeakEQ4" [in_,freq,rs,db] Nothing 1 (Special 0) NoId

-- | 3D Perlin Noise
--
--  Perlin3 [ControlRate,AudioRate] x=0 y=0 z=0
perlin3 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
perlin3 rate x y z = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Perlin3" [x,y,z] Nothing 1 (Special 0) NoId

-- | Sample permutation Ugen.
--
--  PermMod [AudioRate] in=0 freq=100
permMod :: Rate -> Ugen -> Ugen -> Ugen
permMod rate in_ freq = mkUgen Nothing [AudioRate] (Left rate) "PermMod" [in_,freq] Nothing 1 (Special 0) NoId

-- | Sample permutation Ugen with programmable pattern.
--
--  PermModArray [AudioRate] in=0 freq=50 pattern=0
permModArray :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
permModArray rate in_ freq pattern_ = mkUgen Nothing [AudioRate] (Left rate) "PermModArray" [in_,freq,pattern_] Nothing 1 (Special 0) NoId

-- | Sample permutation Ugen with tail.
--
--  PermModT [AudioRate] in=0 outfreq=440 infreq=5000
permModT :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
permModT rate in_ outfreq infreq = mkUgen Nothing [AudioRate] (Left rate) "PermModT" [in_,outfreq,infreq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PhasorModal [AudioRate] input=0 freq=100 decay=0.25 damp=1 amp=0.5 phase=0;    FILTER: TRUE
phasorModal :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
phasorModal input freq decay_ damp amp phase = mkUgen Nothing [AudioRate] (Right [0]) "PhasorModal" [input,freq,decay_,damp,amp,phase] Nothing 1 (Special 0) NoId

-- | Tree classifier using (hyper)planes – Ugen or language-side
--
--  PlaneTree [ControlRate] treebuf=0 in=0 gate=1
planeTree :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
planeTree rate treebuf in_ gate_ = mkUgen Nothing [ControlRate] (Left rate) "PlaneTree" [treebuf,in_,gate_] Nothing 1 (Special 0) NoId

-- | POKEY Chip Sound Simulator
--
--  Pokey [AudioRate] audf1=0 audc1=0 audf2=0 audc2=0 audf3=0 audc3=0 audf4=0 audc4=0 audctl=0
pokey :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pokey rate audf1 audc1 audf2 audc2 audf3 audc3 audf4 audc4 audctl = mkUgen Nothing [AudioRate] (Left rate) "Pokey" [audf1,audc1,audf2,audc2,audf3,audc3,audf4,audc4,audctl] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  PosRatio [AudioRate] in=0 period=100 thresh=0.1
posRatio :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
posRatio rate in_ period thresh = mkUgen Nothing [AudioRate] (Left rate) "PosRatio" [in_,period,thresh] Nothing 1 (Special 0) NoId

-- | debug assistance
--
--  PrintVal [ControlRate] in=0 numblocks=100 id=0
printVal :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
printVal rate in_ numblocks id_ = mkUgen Nothing [ControlRate] (Left rate) "PrintVal" [in_,numblocks,id_] Nothing 1 (Special 0) NoId

-- | constant Q transform pitch follower
--
--  Qitch [ControlRate] in=0 databufnum=0 ampThreshold=0.01 algoflag=1 ampbufnum=0 minfreq=0 maxfreq=2500
qitch :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
qitch rate in_ databufnum ampThreshold algoflag ampbufnum minfreq maxfreq = mkUgen Nothing [ControlRate] (Left rate) "Qitch" [in_,databufnum,ampThreshold,algoflag,ampbufnum,minfreq,maxfreq] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  Bezier [ControlRate,AudioRate] haltAfter=100 dx=0.0001 freq=440 phase=0 *param=0;    MCE=1
bezier :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
bezier rate haltAfter dx freq phase param = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Bezier" [haltAfter,dx,freq,phase] (Just [param]) 1 (Special 0) NoId

-- | rotating clock divider
--
--  RCD [AudioRate] clock=0 rotate=0 reset=0 div=0 spread=0 auto=0 len=0 down=0 gates=0;    FILTER: TRUE
rcd :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rcd clock rotate_ reset div_ spread auto len down gates = mkUgen Nothing [AudioRate] (Right [0]) "RCD" [clock,rotate_,reset,div_,spread,auto,len,down,gates] Nothing 8 (Special 0) NoId

-- | (Undocumented class)
--
--  RDL [AudioRate] numChannels=1 inputArray=0
rdl :: Rate -> Ugen -> Ugen -> Ugen
rdl rate numChannels inputArray = mkUgen Nothing [AudioRate] (Left rate) "RDL" [numChannels,inputArray] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  DX7 [AudioRate] bufnum=0 on=0 off=0 data=0 vc=0 mnn=60 vel=99 pw=0 mw=0 bc=0 fc=0
dx7 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dx7 rate bufnum on off data_ vc mnn vel pw mw bc fc = mkUgen Nothing [AudioRate] (Left rate) "DX7" [bufnum,on,off,data_,vc,mnn,vel,pw,mw,bc,fc] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RDX7Env [AudioRate] gate=0 data=0 r1=0 r2=0 r3=0 r4=0 l1=0 l2=0 l3=0 l4=0 ol=0
rdx7Env :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rdx7Env rate gate_ data_ r1 r2 r3 r4 l1 l2 l3 l4 ol = mkUgen Nothing [AudioRate] (Left rate) "RDX7Env" [gate_,data_,r1,r2,r3,r4,l1,l2,l3,l4,ol] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RDelayMap [AudioRate] bufnum=0 in=0 dynamic=0 *spec=0;    MCE=1, FILTER: TRUE
rDelayMap :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rDelayMap bufnum in_ dynamic spec = mkUgen Nothing [AudioRate] (Right [1]) "RDelayMap" [bufnum,in_,dynamic] (Just [spec]) 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RDelaySet [AudioRate] input=0 *setArray=0;    MCE=1, FILTER: TRUE
rDelaySet :: Ugen -> Ugen -> Ugen
rDelaySet input setArray = mkUgen Nothing [AudioRate] (Right [0]) "RDelaySet" [input] (Just [setArray]) 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RDelaySetBuf [AudioRate] bufnum=0 in=0 spec=0
rDelaySetBuf :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
rDelaySetBuf rate bufnum in_ spec = mkUgen Nothing [AudioRate] (Left rate) "RDelaySetBuf" [bufnum,in_,spec] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  dustRange [AudioRate] iot_min=0.1 iot_max=1
dustRangeId :: ID a => a -> Rate -> Ugen -> Ugen -> Ugen
dustRangeId z rate iot_min iot_max = mkUgen Nothing [AudioRate] (Left rate) "DustRange" [iot_min,iot_max] Nothing 1 (Special 0) (toUid z)

-- | (Undocumented class)
--
--  ExpRandN [InitialisationRate] lo=0 hi=1;    NC INPUT: True, NONDET
expRandNId :: ID a => Int -> a -> Ugen -> Ugen -> Ugen
expRandNId numChannels z lo hi = mkUgen Nothing [InitialisationRate] (Left InitialisationRate) "ExpRandN" [lo,hi] Nothing numChannels (Special 0) (toUid z)

-- | Monad variant of ExpRandN.
expRandNM :: Uid m => Int -> Ugen -> Ugen -> m Ugen
expRandNM nc = liftUid2 (expRandNId nc)

-- | Unsafe variant of ExpRandN.
expRandN ::  Int -> Ugen -> Ugen -> Ugen
expRandN nc = liftUnsafe2 (expRandNM nc)

-- | (Undocumented class)
--
--  Freezer [AudioRate] bufnum=0 left=0 right=1 gain=1 increment=1 incrementOffset=0 incrementRandom=0 rightRandom=0 syncPhaseTrigger=0 randomizePhaseTrigger=0 numberOfLoops=4
freezer :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
freezer bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops = mkUgen Nothing [AudioRate] (Left AudioRate) "Freezer" [bufnum,left,right,gain,increment,incrementOffset,incrementRandom,rightRandom,syncPhaseTrigger,randomizePhaseTrigger,numberOfLoops] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  IRandN [] numChannels=2 lo=0 hi=127
iRandNId :: ID a => a -> Int -> Ugen -> Ugen -> Ugen
iRandNId z numChannels lo hi = mkUgen Nothing [InitialisationRate] (Left InitialisationRate) "IRandN" [lo,hi] Nothing numChannels (Special 0) (toUid z)

-- | Monad variant of IRandN.
iRandNM :: Uid m => Int -> Ugen -> Ugen -> m Ugen
iRandNM nc = liftUid2 (iRandNId nc)

-- | Unsafe variant of IRandN.
iRandN ::  Int -> Ugen -> Ugen -> Ugen
iRandN nc = liftUnsafe2 (iRandNM nc)

-- | TB303 Filter Emulation
--
--  RLPFD [ControlRate,AudioRate] in=0 ffreq=440 res=0 dist=0;    FILTER: TRUE
rlpfd :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rlpfd in_ ffreq res dist = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "RLPFD" [in_,ffreq,res,dist] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RLagC [ControlRate] in=0 timeUp=0.1 curveUp=0 timeDown=0.1 curveDown=0;    FILTER: TRUE
rLagC :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rLagC in_ timeUp curveUp timeDown curveDown = mkUgen Nothing [ControlRate] (Right [0]) "RLagC" [in_,timeUp,curveUp,timeDown,curveDown] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  LinRandN [InitialisationRate] lo=0 hi=1 minmax=0;    NC INPUT: True, NONDET
linRandNId :: ID a => Int -> a -> Ugen -> Ugen -> Ugen -> Ugen
linRandNId numChannels z lo hi minmax = mkUgen Nothing [InitialisationRate] (Left InitialisationRate) "LinRandN" [lo,hi,minmax] Nothing numChannels (Special 0) (toUid z)

-- | Monad variant of LinRandN.
linRandNM :: Uid m => Int -> Ugen -> Ugen -> Ugen -> m Ugen
linRandNM nc = liftUid3 (linRandNId nc)

-- | Unsafe variant of LinRandN.
linRandN ::  Int -> Ugen -> Ugen -> Ugen -> Ugen
linRandN nc = liftUnsafe3 (linRandNM nc)

-- | (Undocumented class)
--
--  RLoopSet [AudioRate] bufnum=0 left=0 right=1 gain=1 increment=1 spec=0
rLoopSet :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rLoopSet rate bufnum left right gain increment spec = mkUgen Nothing [AudioRate] (Left rate) "RLoopSet" [bufnum,left,right,gain,increment,spec] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMAFoodChainL [AudioRate] freq=22050 a1=5 b1=3 d1=0.4 a2=0.1 b2=2 d2=0.01 k=1.0943 r=0.8904 h=0.05 xi=0.1 yi=0 zi=0
rmaFoodChainL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rmaFoodChainL rate freq a1 b1 d1 a2 b2 d2 k r h xi yi zi = mkUgen Nothing [AudioRate] (Left rate) "RMAFoodChainL" [freq,a1,b1,d1,a2,b2,d2,k,r,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  RMEQ [AudioRate] in=0 freq=440 rq=0.1 k=0;    FILTER: TRUE
rmeq :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rmeq in_ freq rq k = mkUgen Nothing [AudioRate] (Right [0]) "RMEQ" [in_,freq,rq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMEQSuite [] maxSize=0
rmeqSuite :: Rate -> Ugen -> Ugen
rmeqSuite rate maxSize = mkUgen Nothing [InitialisationRate,ControlRate,AudioRate,DemandRate] (Left rate) "RMEQSuite" [maxSize] Nothing 1 (Special 0) NoId

-- | root mean square
--
--  RMS [ControlRate,AudioRate] in=0 lpFreq=10
rms :: Rate -> Ugen -> Ugen -> Ugen
rms rate in_ lpFreq = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "RMS" [in_,lpFreq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMShelf [AudioRate] in=0 freq=440 k=0
rmShelf :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
rmShelf rate in_ freq k = mkUgen Nothing [AudioRate] (Left rate) "RMShelf" [in_,freq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RMShelf2 [AudioRate] in=0 freq=440 k=0
rmShelf2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
rmShelf2 rate in_ freq k = mkUgen Nothing [AudioRate] (Left rate) "RMShelf2" [in_,freq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  ObxdFilter [AudioRate] in=0 cutoff=440 resonance=0 multimode=0.5 bandpass=0 fourpole=0
obxdFilter :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
obxdFilter in_ cutoff resonance multimode bandpass fourpole = mkUgen Nothing [AudioRate] (Right [0]) "ObxdFilter" [in_,cutoff,resonance,multimode,bandpass,fourpole] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RPVDecayTbl [] fft_buf=0 decay_rate_buf=0 history_buf=0
rpvDecayTbl :: Ugen -> Ugen -> Ugen -> Ugen
rpvDecayTbl fft_buf decay_rate_buf history_buf = mkUgen Nothing [ControlRate] (Left ControlRate) "RPVDecayTbl" [fft_buf,decay_rate_buf,history_buf] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RandN [InitialisationRate] lo=0 hi=1;    NC INPUT: True, NONDET
randNId :: ID a => Int -> a -> Ugen -> Ugen -> Ugen
randNId numChannels z lo hi = mkUgen Nothing [InitialisationRate] (Left InitialisationRate) "RandN" [lo,hi] Nothing numChannels (Special 0) (toUid z)

-- | Monad variant of RandN.
randNM :: Uid m => Int -> Ugen -> Ugen -> m Ugen
randNM nc = liftUid2 (randNId nc)

-- | Unsafe variant of RandN.
randN ::  Int -> Ugen -> Ugen -> Ugen
randN nc = liftUnsafe2 (randNM nc)

-- | (Undocumented class)
--
--  RSVFBP [AudioRate] in=0 freq=440 q=0
svfBp :: Ugen -> Ugen -> Ugen -> Ugen
svfBp in_ freq q = mkUgen Nothing [AudioRate] (Right [0]) "SvfBp" [in_,freq,q] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SvfHp [AudioRate] in=0 freq=440 q=0
svfHp :: Ugen -> Ugen -> Ugen -> Ugen
svfHp in_ freq q = mkUgen Nothing [AudioRate] (Right [0]) "SvfHp" [in_,freq,q] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SvflP [AudioRate] in=0 freq=440 q=0
svfLp :: Ugen -> Ugen -> Ugen -> Ugen
svfLp in_ freq q = mkUgen Nothing [AudioRate] (Right [0]) "SvfLp" [in_,freq,q] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  ShufflerB [AudioRate] bufnum=0 readLocationMinima=0.01 readLocationMaxima=0.02 readIncrementMinima=1 readIncrementMaxima=1 durationMinima=0.2 durationMaxima=0.2 envelopeAmplitudeMinima=0.5 envelopeAmplitudeMaxima=0.5 envelopeShapeMinima=0.5 envelopeShapeMaxima=0.5 envelopeSkewMinima=0.5 envelopeSkewMaxima=0.5 stereoLocationMinima=0.5 stereoLocationMaxima=0.5 interOffsetTimeMinima=0.05 interOffsetTimeMaxima=0.01 ftableReadLocationIncrement=1 readIncrementQuanta=0 interOffsetTimeQuanta=0
shufflerB :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
shufflerB bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta = mkUgen Nothing [AudioRate] (Left AudioRate) "ShufflerB" [bufnum,readLocationMinima,readLocationMaxima,readIncrementMinima,readIncrementMaxima,durationMinima,durationMaxima,envelopeAmplitudeMinima,envelopeAmplitudeMaxima,envelopeShapeMinima,envelopeShapeMaxima,envelopeSkewMinima,envelopeSkewMaxima,stereoLocationMinima,stereoLocationMaxima,interOffsetTimeMinima,interOffsetTimeMaxima,ftableReadLocationIncrement,readIncrementQuanta,interOffsetTimeQuanta] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  RShufflerL [AudioRate] in=0 fragmentSize=0.01 maxDelay=0.01
rShufflerL :: Ugen -> Ugen -> Ugen -> Ugen
rShufflerL in_ fragmentSize maxDelay = mkUgen Nothing [AudioRate] (Right [0]) "RShufflerL" [in_,fragmentSize,maxDelay] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RSmplrIndex [ControlRate] buf=0 size=0 mnn=60
rSmplrIndex :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
rSmplrIndex rate buf size mnn = mkUgen Nothing [ControlRate] (Left rate) "RSmplrIndex" [buf,size,mnn] Nothing 2 (Special 0) NoId

-- | (Undocumented class)
--
--  TExpRandN [ControlRate] lo=0 hi=1 trigger=0;    NC INPUT: True, FILTER: TRUE, NONDET
tExpRandNId :: ID a => Int -> a -> Ugen -> Ugen -> Ugen -> Ugen
tExpRandNId numChannels z lo hi trigger = mkUgen Nothing [ControlRate] (Right [2]) "TExpRandN" [lo,hi,trigger] Nothing numChannels (Special 0) (toUid z)

-- | Monad variant of TExpRandN.
tExpRandNM :: Uid m => Int -> Ugen -> Ugen -> Ugen -> m Ugen
tExpRandNM nc = liftUid3 (tExpRandNId nc)

-- | Unsafe variant of TExpRandN.
tExpRandN ::  Int -> Ugen -> Ugen -> Ugen -> Ugen
tExpRandN nc = liftUnsafe3 (tExpRandNM nc)

-- | (Undocumented class)
--
--  TLinRandN [ControlRate] lo=0 hi=1 minmax=0 trigger=0;    NC INPUT: True, FILTER: TRUE, NONDET
tLinRandNId :: ID a => Int -> a -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tLinRandNId numChannels z lo hi minmax trigger = mkUgen Nothing [ControlRate] (Right [3]) "TLinRandN" [lo,hi,minmax,trigger] Nothing numChannels (Special 0) (toUid z)

-- | Monad variant of TLinRandN.
tLinRandNM :: Uid m => Int -> Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
tLinRandNM nc = liftUid4 (tLinRandNId nc)

-- | Unsafe variant of TLinRandN.
tLinRandN ::  Int -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tLinRandN nc = liftUnsafe4 (tLinRandNM nc)

-- | (Undocumented class)
--
--  TRandN [ControlRate] lo=0 hi=1 trigger=0;    NC INPUT: True, FILTER: TRUE, NONDET
tRandNId :: ID a => Int -> a -> Ugen -> Ugen -> Ugen -> Ugen
tRandNId numChannels z lo hi trigger = mkUgen Nothing [ControlRate] (Right [2]) "TRandN" [lo,hi,trigger] Nothing numChannels (Special 0) (toUid z)

-- | Monad variant of TRandN.
tRandNM :: Uid m => Int -> Ugen -> Ugen -> Ugen -> m Ugen
tRandNM nc = liftUid3 (tRandNId nc)

-- | Unsafe variant of TRandN.
tRandN ::  Int -> Ugen -> Ugen -> Ugen -> Ugen
tRandN nc = liftUnsafe3 (tRandNM nc)

-- | (Undocumented class)
--
--  TScramble [InitialisationRate,ControlRate] trigger=0 *inputs=0;    MCE=1, FILTER: TRUE, NONDET
tScrambleId :: ID a => a -> Ugen -> Ugen -> Ugen
tScrambleId z trigger inputs = mkUgen Nothing [InitialisationRate,ControlRate] (Right [0]) "TScramble" [trigger] (Just [inputs]) (length (mceChannels inputs) + 0) (Special 0) (toUid z)

-- | Monad variant of TScramble.
tScrambleM :: Uid m => Ugen -> Ugen -> m Ugen
tScrambleM = liftUid2 tScrambleId

-- | Unsafe variant of TScramble.
tScramble ::  Ugen -> Ugen -> Ugen
tScramble = liftUnsafe2 tScrambleM

-- | (Undocumented class)
--
--  RTracePlay [ControlRate,AudioRate] bufnum=0 degree=4 rate=0 axis=1
rTracePlay :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rTracePlay rate bufnum degree rate_ axis = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "RTracePlay" [bufnum,degree,rate_,axis] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RTraceRd [ControlRate,AudioRate] bufnum=0 degree=4 index=0 axis=1
rTraceRd :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rTraceRd rate bufnum degree index_ axis = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "RTraceRd" [bufnum,degree,index_,axis] Nothing 1 (Special 0) NoId

-- | differential pulse-code modulation
--
--  RedDPCMdecode [ControlRate,AudioRate] in=0
redDPCMdecode :: Rate -> Ugen -> Ugen
redDPCMdecode rate in_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "RedDPCMdecode" [in_] Nothing 1 (Special 0) NoId

-- | differential pulse-code modulation
--
--  RedDPCMencode [ControlRate,AudioRate] in=0 round=0
redDPCMencode :: Rate -> Ugen -> Ugen -> Ugen
redDPCMencode rate in_ round_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "RedDPCMencode" [in_,round_] Nothing 1 (Special 0) NoId

-- | look before you leap
--
--  RedLbyl [ControlRate,AudioRate] in=0 thresh=0.5 samples=2
redLbyl :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
redLbyl rate in_ thresh samples = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "RedLbyl" [in_,thresh,samples] Nothing 1 (Special 0) NoId

-- | a really bad pseudo-random noise generator
--
--  RedNoise [ControlRate,AudioRate] clock=0
redNoise :: Rate -> Ugen -> Ugen
redNoise rate clock = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "RedNoise" [clock] Nothing 1 (Special 0) NoId

-- | a phasor that can loop
--
--  RedPhasor [ControlRate,AudioRate] trig=0 rate=1 start=0 end=1 loop=0 loopstart=0 loopend=1
redPhasor :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
redPhasor rate trig_ rate_ start end loop loopstart loopend = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "RedPhasor" [trig_,rate_,start,end,loop,loopstart,loopend] Nothing 1 (Special 0) NoId

-- | a phasor that can loop - version2
--
--  RedPhasor2 [ControlRate,AudioRate] trig=0 rate=1 start=0 end=1 loop=0 loopstart=0 loopend=1
redPhasor2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
redPhasor2 rate trig_ rate_ start end loop loopstart loopend = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "RedPhasor2" [trig_,rate_,start,end,loop,loopstart,loopend] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  RegaliaMitraEQ [AudioRate] in=0 freq=440 rq=0.1 k=0
regaliaMitraEQ :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
regaliaMitraEQ rate in_ freq rq k = mkUgen Nothing [AudioRate] (Left rate) "RegaliaMitraEQ" [in_,freq,rq,k] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Resonator [AudioRate] input=0 freq=100 position=0.001 resolution=24 structure=0.5 brightness=0.5 damping=0.5;    FILTER: TRUE
resonator :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
resonator input freq position resolution structure brightness damping = mkUgen Nothing [AudioRate] (Right [0]) "Resonator" [input,freq,position,resolution,structure,brightness,damping] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Rongs [AudioRate] trigger=0 sustain=1 f0=0.01 structure=0.5 brightness=0.5 damping=0.75 accent=0.9 stretch=0.5 position=0.15 loss=0.15 modeNum=2 cosFreq=0.25
rongs :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rongs rate trigger sustain f0 structure brightness damping accent stretch position loss modeNum cosFreq = mkUgen Nothing [AudioRate] (Left rate) "Rongs" [trigger,sustain,f0,structure,brightness,damping,accent,stretch,position,loss,modeNum,cosFreq] Nothing 1 (Special 0) NoId

-- | Rossler chaotic generator
--
--  RosslerL [AudioRate] freq=22050 a=0.2 b=0.2 c=5.7 h=0.05 xi=0.1 yi=0 zi=0
rosslerL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rosslerL rate freq a b c h xi yi zi = mkUgen Nothing [AudioRate] (Left rate) "RosslerL" [freq,a,b,c,h,xi,yi,zi] Nothing 3 (Special 0) NoId

-- | (Undocumented class)
--
--  RosslerResL [AudioRate] in=0 stiff=1 freq=22050 a=0.2 b=0.2 c=5.7 h=0.05 xi=0.1 yi=0 zi=0
rosslerResL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rosslerResL rate in_ stiff freq a b c h xi yi zi = mkUgen Nothing [AudioRate] (Left rate) "RosslerResL" [in_,stiff,freq,a,b,c,h,xi,yi,zi] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Rotate [AudioRate] w=0 x=0 y=0 z=0 rotate=0
rotate :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
rotate rate w x y z rotate_ = mkUgen Nothing [AudioRate] (Left rate) "Rotate" [w,x,y,z,rotate_] Nothing 1 (Special 0) NoId

-- | (faulty) SID Sound Chip Simulator
--
--  SID6581f [AudioRate] freqLo0=0 freqHi0=0 pwLo0=0 pwHi0=0 ctrl0=0 atkDcy0=0 susRel0=0 freqLo1=0 freqHi1=0 pwLo1=0 pwHi1=0 ctrl1=0 atkDcy1=0 susRel1=0 freqLo2=0 freqHi2=0 pwLo2=0 pwHi2=0 ctrl2=0 atkDcy2=0 susRel2=0 fcLo=0 fcHi=0 resFilt=0 modeVol=0 rate=1
sid6581f :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
sid6581f rate freqLo0 freqHi0 pwLo0 pwHi0 ctrl0 atkDcy0 susRel0 freqLo1 freqHi1 pwLo1 pwHi1 ctrl1 atkDcy1 susRel1 freqLo2 freqHi2 pwLo2 pwHi2 ctrl2 atkDcy2 susRel2 fcLo fcHi resFilt modeVol rate_ = mkUgen Nothing [AudioRate] (Left rate) "SID6581f" [freqLo0,freqHi0,pwLo0,pwHi0,ctrl0,atkDcy0,susRel0,freqLo1,freqHi1,pwLo1,pwHi1,ctrl1,atkDcy1,susRel1,freqLo2,freqHi2,pwLo2,pwHi2,ctrl2,atkDcy2,susRel2,fcLo,fcHi,resFilt,modeVol,rate_] Nothing 1 (Special 0) NoId

-- | experimental time domain onset detector
--
--  SLOnset [ControlRate] input=0 memorysize1=20 before=5 after=5 threshold=10 hysteresis=10
slOnset :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
slOnset rate input memorysize1 before after threshold hysteresis = mkUgen Nothing [ControlRate] (Left rate) "SLOnset" [input,memorysize1,before,after,threshold,hysteresis] Nothing 1 (Special 0) NoId

-- | Spectral Modeling Synthesis
--
--  SMS [AudioRate] input=0 maxpeaks=80 currentpeaks=80 tolerance=4 noisefloor=0.2 freqmult=1 freqadd=0 formantpreserve=0 useifft=0 ampmult=1 graphicsbufnum=0;    FILTER: TRUE
sms :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
sms input maxpeaks currentpeaks tolerance noisefloor freqmult freqadd formantpreserve useifft ampmult graphicsbufnum = mkUgen Nothing [AudioRate] (Right [0]) "SMS" [input,maxpeaks,currentpeaks,tolerance,noisefloor,freqmult,freqadd,formantpreserve,useifft,ampmult,graphicsbufnum] Nothing 2 (Special 0) NoId

-- | Sound Chip Simulator
--
--  SN76489 [AudioRate] tone0=512 tone1=0 tone2=0 noise=0 vol0=15 vol1=0 vol2=0 vol3=0 rate=1
sn76489 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
sn76489 rate tone0 tone1 tone2 noise vol0 vol1 vol2 vol3 rate_ = mkUgen Nothing [AudioRate] (Left rate) "SN76489" [tone0,tone1,tone2,noise,vol0,vol1,vol2,vol3,rate_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SOMAreaWr [ControlRate] bufnum=0 inputdata=0 coords=0 netsize=10 numdims=2 nhood=0.5 gate=1
somAreaWr :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
somAreaWr rate bufnum inputdata coords netsize numdims nhood gate_ = mkUgen Nothing [ControlRate] (Left rate) "SOMAreaWr" [bufnum,inputdata,coords,netsize,numdims,nhood,gate_] Nothing 1 (Special 0) NoId

-- | Map an input using a Self-Organising Map
--
--  SOMRd [ControlRate,AudioRate] bufnum=0 inputdata=0 netsize=10 numdims=2 gate=1
somRd :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
somRd rate bufnum inputdata netsize numdims gate_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "SOMRd" [bufnum,inputdata,netsize,numdims,gate_] Nothing 2 (Special 0) NoId

-- | Create (train) a Self-Organising Map
--
--  SOMTrain [ControlRate] bufnum=0 inputdata=0 netsize=10 numdims=2 traindur=5000 nhood=0.5 gate=1 initweight=1
somTrain :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
somTrain rate bufnum inputdata netsize numdims traindur nhood gate_ initweight = mkUgen Nothing [ControlRate] (Left rate) "SOMTrain" [bufnum,inputdata,netsize,numdims,traindur,nhood,gate_,initweight] Nothing 3 (Special 0) NoId

-- | 12db/Oct State Variable Filter
--
--  SVF [ControlRate,AudioRate] signal=0 cutoff=2200 res=0.1 lowpass=1 bandpass=0 highpass=0 notch=0 peak=0;    FILTER: TRUE
svf :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
svf signal cutoff res lowpass bandpass highpass notch peak_ = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "SVF" [signal,cutoff,res,lowpass,bandpass,highpass,notch,peak_] Nothing 1 (Special 0) NoId

-- | super-efficient sawtooth oscillator with low aliasing
--
--  SawDPW [ControlRate,AudioRate] freq=440 iphase=0
sawDPW :: Rate -> Ugen -> Ugen -> Ugen
sawDPW rate freq iphase = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "SawDPW" [freq,iphase] Nothing 1 (Special 0) NoId

-- | Perceptual feature modeling sensory dissonance
--
--  SensoryDissonance [ControlRate] fft=0 maxpeaks=100 peakthreshold=0.1 norm=0 clamp=1
sensoryDissonance :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
sensoryDissonance rate fft_ maxpeaks peakthreshold norm clamp = mkUgen Nothing [ControlRate] (Left rate) "SensoryDissonance" [fft_,maxpeaks,peakthreshold,norm,clamp] Nothing 1 (Special 0) NoId

-- | Fuzzy sieve based synthesis
--
--  Sieve1 [ControlRate,AudioRate] bufnum=0 gap=2 alternate=1
sieve1 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
sieve1 rate bufnum gap alternate = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Sieve1" [bufnum,gap,alternate] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains
--
--  SinGrain [AudioRate] trigger=0 dur=1 freq=440
sinGrain :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
sinGrain rate trigger dur freq = mkUgen Nothing [AudioRate] (Left rate) "SinGrain" [trigger,dur,freq] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains and user supplied envelope
--
--  SinGrainB [AudioRate] trigger=0 dur=1 freq=440 envbuf=0
sinGrainB :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
sinGrainB rate trigger dur freq envbuf = mkUgen Nothing [AudioRate] (Left rate) "SinGrainB" [trigger,dur,freq,envbuf] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains with Ambisonic panning and user supplied envelope
--
--  SinGrainBBF [AudioRate] trigger=0 dur=1 freq=440 envbuf=0 azimuth=0 elevation=0 rho=1 wComp=0
sinGrainBBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
sinGrainBBF rate trigger dur freq envbuf azimuth elevation rho wComp = mkUgen Nothing [AudioRate] (Left rate) "SinGrainBBF" [trigger,dur,freq,envbuf,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains with Ambisonic panning
--
--  SinGrainBF [AudioRate] trigger=0 dur=1 freq=440 azimuth=0 elevation=0 rho=1 wComp=0
sinGrainBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
sinGrainBF rate trigger dur freq azimuth elevation rho wComp = mkUgen Nothing [AudioRate] (Left rate) "SinGrainBF" [trigger,dur,freq,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains and user supplied envelopes
--
--  SinGrainI [AudioRate] trigger=0 dur=1 freq=440 envbuf1=0 envbuf2=0 ifac=0.5
sinGrainI :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
sinGrainI rate trigger dur freq envbuf1 envbuf2 ifac = mkUgen Nothing [AudioRate] (Left rate) "SinGrainI" [trigger,dur,freq,envbuf1,envbuf2,ifac] Nothing 1 (Special 0) NoId

-- | Granular synthesis with sinusoidal grains with Ambisonic panning and user supplied envelopes
--
--  SinGrainIBF [AudioRate] trigger=0 dur=1 freq=440 envbuf1=0 envbuf2=0 ifac=0.5 azimuth=0 elevation=0 rho=1 wComp=0
sinGrainIBF :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
sinGrainIBF rate trigger dur freq envbuf1 envbuf2 ifac azimuth elevation rho wComp = mkUgen Nothing [AudioRate] (Left rate) "SinGrainIBF" [trigger,dur,freq,envbuf1,envbuf2,ifac,azimuth,elevation,rho,wComp] Nothing 4 (Special 0) NoId

-- | (Undocumented class)
--
--  SinTone [AudioRate] freq=440 phase=0
sinTone :: Rate -> Ugen -> Ugen -> Ugen
sinTone rate freq phase = mkUgen Nothing [AudioRate] (Left rate) "SinTone" [freq,phase] Nothing 1 (Special 0) NoId

-- | port of some ladspa plugins
--
--  SineShaper [AudioRate] in=0 limit=1;    FILTER: TRUE
sineShaper :: Ugen -> Ugen -> Ugen
sineShaper in_ limit = mkUgen Nothing [AudioRate] (Right [0]) "SineShaper" [in_,limit] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SkipNeedle [AudioRate] range=44100 rate=10 offset=0
skipNeedle :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
skipNeedle rate range rate_ offset = mkUgen Nothing [AudioRate] (Left rate) "SkipNeedle" [range,rate_,offset] Nothing 1 (Special 0) NoId

-- | lowpass filter for envelope following
--
--  Slide [ControlRate,AudioRate] in=0 slideup=50 slidedown=3000
slide :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
slide rate in_ slideup slidedown = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Slide" [in_,slideup,slidedown] Nothing 1 (Special 0) NoId

-- | generate cpu spikes
--
--  Slub [ControlRate,AudioRate] trig=0 spike=4.04
slub :: Rate -> Ugen -> Ugen -> Ugen
slub rate trig_ spike = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Slub" [trig_,spike] Nothing 1 (Special 0) NoId

-- | Smooth samplerate and bitrate reduction
--
--  SmoothDecimator [AudioRate] in=0 rate=44100 smoothing=0.5
smoothDecimator :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
smoothDecimator rate in_ rate_ smoothing = mkUgen Nothing [AudioRate] (Left rate) "SmoothDecimator" [in_,rate_,smoothing] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipAmp [AudioRate] in=0 pregain=1;    FILTER: TRUE
softClipAmp :: Ugen -> Ugen -> Ugen
softClipAmp in_ pregain = mkUgen Nothing [AudioRate] (Right [0]) "SoftClipAmp" [in_,pregain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipAmp4 [AudioRate] in=0 pregain=1;    FILTER: TRUE
softClipAmp4 :: Ugen -> Ugen -> Ugen
softClipAmp4 in_ pregain = mkUgen Nothing [AudioRate] (Right [0]) "SoftClipAmp4" [in_,pregain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipAmp8 [AudioRate] in=0 pregain=1;    FILTER: TRUE
softClipAmp8 :: Ugen -> Ugen -> Ugen
softClipAmp8 in_ pregain = mkUgen Nothing [AudioRate] (Right [0]) "SoftClipAmp8" [in_,pregain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipper4 [AudioRate] in=0
softClipper4 :: Rate -> Ugen -> Ugen
softClipper4 rate in_ = mkUgen Nothing [AudioRate] (Left rate) "SoftClipper4" [in_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SoftClipper8 [AudioRate] in=0
softClipper8 :: Rate -> Ugen -> Ugen
softClipper8 rate in_ = mkUgen Nothing [AudioRate] (Left rate) "SoftClipper8" [in_] Nothing 1 (Special 0) NoId

-- | LPC analizer.
--
--  SonLPC [AudioRate] buff=-1.0 in=0.0 hop=0.5 poles=10.0
sonLPC :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
sonLPC rate buff in_ hop poles = mkUgen Nothing [AudioRate] (Left rate) "SonLPC" [buff,in_,hop,poles] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SonLPCSynth [AudioRate] chain=-1.0
sonLPCSynth :: Rate -> Ugen -> Ugen
sonLPCSynth rate chain = mkUgen Nothing [AudioRate] (Left rate) "SonLPCSynth" [chain] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  SonLPCSynthIn [AudioRate] chain=-1.0 in=0.0
sonLPCSynthIn :: Rate -> Ugen -> Ugen -> Ugen
sonLPCSynthIn rate chain in_ = mkUgen Nothing [AudioRate] (Left rate) "SonLPCSynthIn" [chain,in_] Nothing 1 (Special 0) NoId

-- | Karplus-Strong via a sorting algorithm
--
--  SortBuf [AudioRate] bufnum=0 sortrate=10 reset=0
sortBuf :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
sortBuf rate bufnum sortrate reset = mkUgen Nothing [AudioRate] (Left rate) "SortBuf" [bufnum,sortrate,reset] Nothing 1 (Special 0) NoId

-- | Spectral feature extraction
--
--  SpectralEntropy [ControlRate] fft=0 fftsize=2048 numbands=1;    NC INPUT: True
spectralEntropy :: Int -> Rate -> Ugen -> Ugen -> Ugen -> Ugen
spectralEntropy numChannels rate fft_ fftsize numbands = mkUgen Nothing [ControlRate] (Left rate) "SpectralEntropy" [fft_,fftsize,numbands] Nothing numChannels (Special 0) NoId

-- | (Undocumented class)
--
--  Spreader [AudioRate] in=0 theta=1.5708 filtsPerOctave=8
spreader :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
spreader rate in_ theta filtsPerOctave = mkUgen Nothing [AudioRate] (Left rate) "Spreader" [in_,theta,filtsPerOctave] Nothing 2 (Special 0) NoId

-- | Spruce bud worm model equations
--
--  SpruceBudworm [AudioRate] reset=0 rate=0.1 k1=27.9 k2=1.5 alpha=0.1 beta=10.1 mu=0.3 rho=10.1 initx=0.9 inity=0.1
spruceBudworm :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
spruceBudworm rate reset rate_ k1 k2 alpha beta mu rho initx inity = mkUgen Nothing [AudioRate] (Left rate) "SpruceBudworm" [reset,rate_,k1,k2,alpha,beta,mu,rho,initx,inity] Nothing 2 (Special 0) NoId

-- | Wave squeezer. Maybe a kind of pitch shifter.
--
--  Squiz [ControlRate,AudioRate] in=0 pitchratio=2 zcperchunk=1 memlen=0.1;    FILTER: TRUE
squiz :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
squiz in_ pitchratio zcperchunk memlen = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "Squiz" [in_,pitchratio,zcperchunk,memlen] Nothing 1 (Special 0) NoId

-- | standard map 2D chaotic generator
--
--  Standard2DC [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 k=1.4 x0=4.97898 y0=5.74734
standard2DC :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
standard2DC rate minfreq maxfreq k x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Standard2DC" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | standard map 2D chaotic generator
--
--  Standard2DL [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 k=1.4 x0=4.97898 y0=5.74734
standard2DL :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
standard2DL rate minfreq maxfreq k x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Standard2DL" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | standard map 2D chaotic generator
--
--  Standard2DN [ControlRate,AudioRate] minfreq=11025 maxfreq=22050 k=1.4 x0=4.97898 y0=5.74734
standard2DN :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
standard2DN rate minfreq maxfreq k x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "Standard2DN" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StandardTrig [ControlRate,AudioRate] minfreq=5 maxfreq=10 k=1.4 x0=4.97898 y0=5.74734
standardTrig :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
standardTrig rate minfreq maxfreq k x0 y0 = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StandardTrig" [minfreq,maxfreq,k,x0,y0] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBandedWG [ControlRate,AudioRate] freq=440 instr=0 bowpressure=0 bowmotion=0 integration=0 modalresonance=64 bowvelocity=0 setstriking=0 trig=1
stkBandedWG :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkBandedWG rate freq instr bowpressure bowmotion integration modalresonance bowvelocity setstriking trig_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkBandedWG" [freq,instr,bowpressure,bowmotion,integration,modalresonance,bowvelocity,setstriking,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBeeThree [ControlRate,AudioRate] freq=440 op4gain=10 op3gain=20 lfospeed=64 lfodepth=0 adsrtarget=64 trig=1
stkBeeThree :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkBeeThree rate freq op4gain op3gain lfospeed lfodepth adsrtarget trig_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkBeeThree" [freq,op4gain,op3gain,lfospeed,lfodepth,adsrtarget,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBlowHole [ControlRate,AudioRate] freq=440 reedstiffness=64 noisegain=20 tonehole=64 register=11 breathpressure=64
stkBlowHole :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkBlowHole rate freq reedstiffness noisegain tonehole register breathpressure = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkBlowHole" [freq,reedstiffness,noisegain,tonehole,register,breathpressure] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkBowed [ControlRate,AudioRate] freq=220 bowpressure=64 bowposition=64 vibfreq=64 vibgain=64 loudness=64 gate=1 attackrate=1 decayrate=1
stkBowed :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkBowed rate freq bowpressure bowposition vibfreq vibgain loudness_ gate_ attackrate decayrate = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkBowed" [freq,bowpressure,bowposition,vibfreq,vibgain,loudness_,gate_,attackrate,decayrate] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkClarinet [ControlRate,AudioRate] freq=440 reedstiffness=64 noisegain=4 vibfreq=64 vibgain=11 breathpressure=64 trig=1
stkClarinet :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkClarinet rate freq reedstiffness noisegain vibfreq vibgain breathpressure trig_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkClarinet" [freq,reedstiffness,noisegain,vibfreq,vibgain,breathpressure,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkFlute [ControlRate,AudioRate] freq=440 jetDelay=49 noisegain=0.15 jetRatio=0.32
stkFlute :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkFlute rate freq jetDelay noisegain jetRatio = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkFlute" [freq,jetDelay,noisegain,jetRatio] Nothing 1 (Special 0) NoId

-- | Wrapping Synthesis toolkit.
--
--  StkGlobals [AudioRate] showWarnings=0 printErrors=0 rawfilepath=0
stkGlobals :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
stkGlobals rate showWarnings printErrors rawfilepath = mkUgen Nothing [AudioRate] (Left rate) "StkGlobals" [showWarnings,printErrors,rawfilepath] Nothing 1 (Special 0) NoId

-- | Wrapping Synthesis toolkit.
--
--  StkInst [AudioRate] freq=220 gate=1 onamp=1 offamp=0.5 instNumber=6 *args=0;    MCE=1, REORDERS INPUTS: [4,0,1,2,3,5]
stkInst :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkInst rate freq gate_ onamp offamp instNumber args = mkUgen Nothing [AudioRate] (Left rate) "StkInst" [freq,gate_,onamp,offamp,instNumber] (Just [args]) 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkMandolin [ControlRate,AudioRate] freq=520 bodysize=64 pickposition=64 stringdamping=69 stringdetune=10 aftertouch=64 trig=1
stkMandolin :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkMandolin rate freq bodysize pickposition stringdamping stringdetune aftertouch trig_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkMandolin" [freq,bodysize,pickposition,stringdamping,stringdetune,aftertouch,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkModalBar [ControlRate,AudioRate] freq=440 instrument=0 stickhardness=64 stickposition=64 vibratogain=20 vibratofreq=20 directstickmix=64 volume=64 trig=1
stkModalBar :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkModalBar rate freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume trig_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkModalBar" [freq,instrument,stickhardness,stickposition,vibratogain,vibratofreq,directstickmix,volume,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkMoog [ControlRate,AudioRate] freq=440 filterQ=10 sweeprate=20 vibfreq=64 vibgain=0 gain=64 trig=1
stkMoog :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkMoog rate freq filterQ sweeprate vibfreq vibgain gain trig_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkMoog" [freq,filterQ,sweeprate,vibfreq,vibgain,gain,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkPluck [ControlRate,AudioRate] freq=440 decay=0.99
stkPluck :: Rate -> Ugen -> Ugen -> Ugen
stkPluck rate freq decay_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkPluck" [freq,decay_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkSaxofony [ControlRate,AudioRate] freq=220 reedstiffness=64 reedaperture=64 noisegain=20 blowposition=26 vibratofrequency=20 vibratogain=20 breathpressure=128 trig=1
stkSaxofony :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkSaxofony rate freq reedstiffness reedaperture noisegain blowposition vibratofrequency vibratogain breathpressure trig_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkSaxofony" [freq,reedstiffness,reedaperture,noisegain,blowposition,vibratofrequency,vibratogain,breathpressure,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkShakers [ControlRate,AudioRate] instr=0 energy=64 decay=64 objects=64 resfreq=64
stkShakers :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkShakers rate instr energy decay_ objects resfreq = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkShakers" [instr,energy,decay_,objects,resfreq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StkVoicForm [ControlRate,AudioRate] freq=440 vuvmix=64 vowelphon=64 vibfreq=64 vibgain=20 loudness=64 trig=1
stkVoicForm :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkVoicForm rate freq vuvmix vowelphon vibfreq vibgain loudness_ trig_ = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "StkVoicForm" [freq,vuvmix,vowelphon,vibfreq,vibgain,loudness_,trig_] Nothing 1 (Special 0) NoId

-- | String resonance filter
--
--  Streson [ControlRate,AudioRate] input=0 delayTime=0.003 res=0.9;    FILTER: TRUE
streson :: Ugen -> Ugen -> Ugen -> Ugen
streson input delayTime res = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "Streson" [input,delayTime,res] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  StringVoice [AudioRate] trig=0 infsustain=0 freq=100 accent=0.5 structure=0.5 brightness=0.5 damping=0.5
stringVoice :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stringVoice rate trig_ infsustain freq accent structure brightness damping = mkUgen Nothing [AudioRate] (Left rate) "StringVoice" [trig_,infsustain,freq,accent,structure,brightness,damping] Nothing 1 (Special 0) NoId

-- | Pulse counter with floating point steps
--
--  Summer [ControlRate,AudioRate] trig=0 step=1 reset=0 resetval=0;    FILTER: TRUE
summer :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
summer trig_ step reset resetval = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "Summer" [trig_,step,reset,resetval] Nothing 1 (Special 0) NoId

-- | feedback delay line implementing switch-and-ramp buffer jumping
--
--  SwitchDelay [AudioRate] in=0 drylevel=1 wetlevel=1 delaytime=1 delayfactor=0.7 maxdelaytime=20;    FILTER: TRUE
switchDelay :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
switchDelay in_ drylevel wetlevel delaytime delayfactor maxdelaytime = mkUgen Nothing [AudioRate] (Right [0]) "SwitchDelay" [in_,drylevel,wetlevel,delaytime,delayfactor,maxdelaytime] Nothing 1 (Special 0) NoId

-- | triggered beta random distribution
--
--  TBetaRand [ControlRate,AudioRate] lo=0 hi=1 prob1=0 prob2=0 trig=0;    FILTER: TRUE, NONDET
tBetaRandId :: ID a => a -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tBetaRandId z lo hi prob1 prob2 trig_ = mkUgen Nothing [ControlRate,AudioRate] (Right [4]) "TBetaRand" [lo,hi,prob1,prob2,trig_] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of TBetaRand.
tBetaRandM :: Uid m => Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
tBetaRandM = liftUid5 tBetaRandId

-- | Unsafe variant of TBetaRand.
tBetaRand ::  Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tBetaRand = liftUnsafe5 tBetaRandM

-- | triggered random walk generator
--
--  TBrownRand [ControlRate,AudioRate] lo=0 hi=1 dev=1 dist=0 trig=0;    FILTER: TRUE, NONDET
tBrownRandId :: ID a => a -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tBrownRandId z lo hi dev dist trig_ = mkUgen Nothing [ControlRate,AudioRate] (Right [4]) "TBrownRand" [lo,hi,dev,dist,trig_] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of TBrownRand.
tBrownRandM :: Uid m => Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
tBrownRandM = liftUid5 tBrownRandId

-- | Unsafe variant of TBrownRand.
tBrownRand ::  Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tBrownRand = liftUnsafe5 tBrownRandM

-- | triggered gaussian random distribution
--
--  TGaussRand [ControlRate,AudioRate] lo=0 hi=1 trig=0;    FILTER: TRUE, NONDET
tGaussRandId :: ID a => a -> Ugen -> Ugen -> Ugen -> Ugen
tGaussRandId z lo hi trig_ = mkUgen Nothing [ControlRate,AudioRate] (Right [2]) "TGaussRand" [lo,hi,trig_] Nothing 1 (Special 0) (toUid z)

-- | Monad variant of TGaussRand.
tGaussRandM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
tGaussRandM = liftUid3 tGaussRandId

-- | Unsafe variant of TGaussRand.
tGaussRand ::  Ugen -> Ugen -> Ugen -> Ugen
tGaussRand = liftUnsafe3 tGaussRandM

-- | buffer granulator with linear att/dec
--
--  TGrains2 [AudioRate] trigger=0 bufnum=0 rate=1 centerPos=0 dur=0.1 pan=0 amp=0.1 att=0.5 dec=0.5 interp=4;    NC INPUT: True
tGrains2 :: Int -> Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tGrains2 numChannels rate trigger bufnum rate_ centerPos dur pan amp att dec interp = mkUgen Nothing [AudioRate] (Left rate) "TGrains2" [trigger,bufnum,rate_,centerPos,dur,pan,amp,att,dec,interp] Nothing numChannels (Special 0) NoId

-- | buffer granulator with user envelope
--
--  TGrains3 [AudioRate] trigger=0 bufnum=0 rate=1 centerPos=0 dur=0.1 pan=0 amp=0.1 att=0.5 dec=0.5 window=1 interp=4;    NC INPUT: True
tGrains3 :: Int -> Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tGrains3 numChannels rate trigger bufnum rate_ centerPos dur pan amp att dec window interp = mkUgen Nothing [AudioRate] (Left rate) "TGrains3" [trigger,bufnum,rate_,centerPos,dur,pan,amp,att,dec,window,interp] Nothing numChannels (Special 0) NoId

-- | Tracking Phase Vocoder
--
--  TPV [AudioRate] chain=0 windowsize=1024 hopsize=512 maxpeaks=80 currentpeaks=0 freqmult=1 tolerance=4 noisefloor=0.2
tpv :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tpv chain windowsize hopsize maxpeaks currentpeaks freqmult tolerance noisefloor = mkUgen Nothing [AudioRate] (Left AudioRate) "TPV" [chain,windowsize,hopsize,maxpeaks,currentpeaks,freqmult,tolerance,noisefloor] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  TTendency [ControlRate,AudioRate] trigger=0 dist=0 parX=0 parY=1 parA=0 parB=0
tTendency :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tTendency rate trigger dist parX parY parA parB = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "TTendency" [trigger,dist,parX,parY,parA,parB] Nothing 1 (Special 0) NoId

-- | pitch tracker
--
--  Tartini [ControlRate] in=0 threshold=0.93 n=2048 k=0 overlap=1024 smallCutoff=0.5
tartini :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tartini rate in_ threshold n k overlap smallCutoff = mkUgen Nothing [ControlRate] (Left rate) "Tartini" [in_,threshold,n,k,overlap,smallCutoff] Nothing 2 (Special 0) NoId

-- | Neural Oscillator
--
--  TermanWang [AudioRate] input=0 reset=0 ratex=0.01 ratey=0.01 alpha=1 beta=1 eta=1 initx=0 inity=0
termanWang :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
termanWang rate input reset ratex ratey alpha beta eta initx inity = mkUgen Nothing [AudioRate] (Left rate) "TermanWang" [input,reset,ratex,ratey,alpha,beta,eta,initx,inity] Nothing 1 (Special 0) NoId

-- | display level of a Ugen as a textual meter
--
--  TextVU [ControlRate,AudioRate] trig=2 in=0 label=0 width=21 reset=0 ana=0
textVU ::  Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
textVU rate trig_ in_ label_ width reset ana = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "TextVU" [trig_,in_,label_,width,reset,ana] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Tilt [AudioRate] w=0 x=0 y=0 z=0 tilt=0
tilt :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tilt rate w x y z tilt_ = mkUgen Nothing [AudioRate] (Left rate) "Tilt" [w,x,y,z,tilt_] Nothing 1 (Special 0) NoId

-- | triggered signal averager
--
--  TrigAvg [ControlRate] in=0 trig=0
trigAvg :: Rate -> Ugen -> Ugen -> Ugen
trigAvg rate in_ trig_ = mkUgen Nothing [ControlRate] (Left rate) "TrigAvg" [in_,trig_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Tumble [AudioRate] w=0 x=0 y=0 z=0 tilt=0
tumble :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tumble rate w x y z tilt_ = mkUgen Nothing [AudioRate] (Left rate) "Tumble" [w,x,y,z,tilt_] Nothing 1 (Special 0) NoId

-- | physical modeling simulation; two tubes
--
--  TwoTube [AudioRate] input=0 k=0.01 loss=1 d1length=100 d2length=100
twoTube :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
twoTube rate input k loss d1length d2length = mkUgen Nothing [AudioRate] (Left rate) "TwoTube" [input,k,loss,d1length,d2length] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  UHJ2B [AudioRate] ls=0 rs=0
uhj2b :: Rate -> Ugen -> Ugen -> Ugen
uhj2b rate ls rs = mkUgen Nothing [AudioRate] (Left rate) "UHJ2B" [ls,rs] Nothing 3 (Special 0) NoId

-- | Vector Base Amplitude Panner
--
--  VBAP [ControlRate,AudioRate] in=0 bufnum=0 azimuth=0 elevation=1 spread=0;    NC INPUT: True
vbap :: Int -> Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
vbap numChannels rate in_ bufnum azimuth elevation spread = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "VBAP" [in_,bufnum,azimuth,elevation,spread] Nothing numChannels (Special 0) NoId

-- | a Chebyshev low/highpass filter
--
--  VBChebyFilt [AudioRate] in=0 freq=880 mode=0 order=4
vbChebyFilt :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
vbChebyFilt rate in_ freq mode order = mkUgen Nothing [AudioRate] (Left rate) "VBChebyFilt" [in_,freq,mode,order] Nothing 1 (Special 0) NoId

-- | a chaotic oscillator network
--
--  VBFourses [AudioRate] smoother=0.5 *freqarray=0;    MCE=1, REORDERS INPUTS: [1,0]
vbFourses :: Rate -> Ugen -> Ugen -> Ugen
vbFourses rate smoother freqarray = mkUgen Nothing [AudioRate] (Left rate) "VBFourses" [smoother] (Just [freqarray]) 4 (Special 0) NoId

-- | artifical reverberator
--
--  VBJonVerb [AudioRate] in=0 decay=0.6 damping=0.3 inputbw=0.8 erfl=0.5 tail=0.5;    FILTER: TRUE
vbJonVerb :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
vbJonVerb in_ decay_ damping inputbw erfl tail_ = mkUgen Nothing [AudioRate] (Right [0]) "VBJonVerb" [in_,decay_,damping,inputbw,erfl,tail_] Nothing 2 (Special 0) NoId

-- | a simple phase vocoder for time-stretching
--
--  VBPVoc [AudioRate] numChannels=0 bufnum=0 playpos=0 fftsize=2048
vbpVoc :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
vbpVoc rate numChannels bufnum playpos fftsize = mkUgen Nothing [AudioRate] (Left rate) "VBPVoc" [numChannels,bufnum,playpos,fftsize] Nothing 1 (Special 0) NoId

-- | lowpass filter for envelope following
--
--  VBSlide [KR,AR] in=0.0 slideup=50.0 slidedown=3000.0;    FILTER: TRUE
vbSlide :: Ugen -> Ugen -> Ugen -> Ugen
vbSlide in_ slideup slidedown = mkUgen Nothing [ControlRate,AudioRate] (Right [0]) "VBSlide" [in_,slideup,slidedown] Nothing 1 (Special 0) NoId

-- | 2D scanning pattern virtual machine
--
--  VMScan2D [AudioRate] bufnum=0
vmScan2D :: Rate -> Ugen -> Ugen
vmScan2D rate bufnum = mkUgen Nothing [AudioRate] (Left rate) "VMScan2D" [bufnum] Nothing 2 (Special 0) NoId

-- | vosim pulse generator
--
--  VOSIM [AudioRate] trig=0.1 freq=400 nCycles=1 decay=0.9
vosim :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
vosim rate trig_ freq nCycles decay_ = mkUgen Nothing [AudioRate] (Left rate) "VOSIM" [trig_,freq,nCycles,decay_] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  VarShapeOsc [ControlRate,AudioRate] freq=100 pw=0.5 waveshape=0.5 sync=1 syncfreq=105
varShapeOsc :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
varShapeOsc rate freq pw waveshape sync syncfreq = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "VarShapeOsc" [freq,pw,waveshape,sync,syncfreq] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  VosimOsc [ControlRate,AudioRate] freq=100 form1freq=951 form2freq=919 shape=0
vosimOsc :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
vosimOsc rate freq form1freq form2freq shape = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "VosimOsc" [freq,form1freq,form2freq,shape] Nothing 1 (Special 0) NoId

-- | windowed amplitude follower
--
--  WAmp [ControlRate] in=0 winSize=0.1
wAmp :: Rate -> Ugen -> Ugen -> Ugen
wAmp rate in_ winSize = mkUgen Nothing [ControlRate] (Left rate) "WAmp" [in_,winSize] Nothing 1 (Special 0) NoId

-- | decomposition into square waves, and reconstruction
--
--  WalshHadamard [AudioRate] input=0 which=0
walshHadamard :: Rate -> Ugen -> Ugen -> Ugen
walshHadamard rate input which = mkUgen Nothing [AudioRate] (Left rate) "WalshHadamard" [input,which] Nothing 1 (Special 0) NoId

-- | Warp a buffer with a time pointer
--
--  WarpZ [AudioRate] bufnum=0 pointer=0 freqScale=1 windowSize=0.2 envbufnum=-1 overlaps=8 windowRandRatio=0 interp=1 zeroSearch=0 zeroStart=0;    NC INPUT: True
warpZ :: Int -> Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
warpZ numChannels rate bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp zeroSearch zeroStart = mkUgen Nothing [AudioRate] (Left rate) "WarpZ" [bufnum,pointer,freqScale,windowSize,envbufnum,overlaps,windowRandRatio,interp,zeroSearch,zeroStart] Nothing numChannels (Special 0) NoId

-- | Lose bits of your waves
--
--  WaveLoss [ControlRate,AudioRate] in=0 drop=20 outof=40 mode=1
waveLoss :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
waveLoss rate in_ drop_ outof mode = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "WaveLoss" [in_,drop_,outof,mode] Nothing 1 (Special 0) NoId

-- | wave terrain synthesis
--
--  WaveTerrain [AudioRate] bufnum=0 x=0 y=0 xsize=100 ysize=100
waveTerrain :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
waveTerrain rate bufnum x y xsize ysize = mkUgen Nothing [AudioRate] (Left rate) "WaveTerrain" [bufnum,x,y,xsize,ysize] Nothing 1 (Special 0) NoId

-- | decomposition into Daub4 wavelets, and reconstruction
--
--  WaveletDaub [AudioRate] input=0 n=64 which=0
waveletDaub :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
waveletDaub rate input n which = mkUgen Nothing [AudioRate] (Left rate) "WaveletDaub" [input,n,which] Nothing 1 (Special 0) NoId

-- | Weakly Nonlinear Oscillator
--
--  WeaklyNonlinear [AudioRate] input=0 reset=0 ratex=1 ratey=1 freq=440 initx=0 inity=0 alpha=0 xexponent=0 beta=0 yexponent=0
weaklyNonlinear :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
weaklyNonlinear rate input reset ratex ratey freq initx inity alpha xexponent beta yexponent = mkUgen Nothing [AudioRate] (Left rate) "WeaklyNonlinear" [input,reset,ratex,ratey,freq,initx,inity,alpha,xexponent,beta,yexponent] Nothing 1 (Special 0) NoId

-- | Weakly Nonlinear Oscillator
--
--  WeaklyNonlinear2 [AudioRate] input=0 reset=0 ratex=1 ratey=1 freq=440 initx=0 inity=0 alpha=0 xexponent=0 beta=0 yexponent=0
weaklyNonlinear2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
weaklyNonlinear2 rate input reset ratex ratey freq initx inity alpha xexponent beta yexponent = mkUgen Nothing [AudioRate] (Left rate) "WeaklyNonlinear2" [input,reset,ratex,ratey,freq,initx,inity,alpha,xexponent,beta,yexponent] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  Werner [AudioRate] input=0 freq=0.5 damp=0.5 feedback=0.5 drive=0 oversample=1
werner :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
werner rate input freq damp feedback drive oversample = mkUgen Nothing [AudioRate] (Left rate) "Werner" [input,freq,damp,feedback,drive,oversample] Nothing 1 (Special 0) NoId

-- | Pulse counter with floating point steps
--
--  WrapSummer [ControlRate,AudioRate] trig=0 step=1 min=0 max=1 reset=0 resetval=0
wrapSummer :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
wrapSummer rate trig_ step min_ max_ reset resetval = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "WrapSummer" [trig_,step,min_,max_,reset,resetval] Nothing 1 (Special 0) NoId

-- | (Undocumented class)
--
--  ZOsc [ControlRate,AudioRate] freq=100 formantfreq=91 shape=0.5 mode=0.5
zOsc :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
zOsc rate freq formantfreq shape mode = mkUgen Nothing [ControlRate,AudioRate] (Left rate) "ZOsc" [freq,formantfreq,shape,mode] Nothing 1 (Special 0) NoId
