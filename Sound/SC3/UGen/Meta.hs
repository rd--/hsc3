module Sound.SC3.UGen.Meta where

import Data.Maybe {- base -}

import Sound.SC3.UGen.Rate {- hsc3 -}

-- | Indices are sclang indices.
meta_nc_input :: [(String,Int)]
meta_nc_input =
    [("BufRd",0)
    ,("DecodeB2",0)
    ,("DiskIn",0)
    ,("GrainBuf",0)
    ,("GrainFM",0)
    ,("GrainIn",0)
    ,("GrainSin",0)
    ,("In",1)
    ,("InFeedback",1)
    ,("InTrig",1)
    ,("LagIn",0)
    ,("LocalIn",0)
    ,("LoopBuf",0)
    ,("DecodeB2",0)
    ,("PanAz",0)
    ,("TGrains",0)
    ,("TGrains2",0)
    ,("TGrains3",0)
    ,("Tap",0)
    ,("VBAP",0)
    ,("VDiskIn",0)
    ,("Warp1",0)
    ,("WarpZ",0)]

meta_input_reorder :: [(String,[Int])]
meta_input_reorder =
    [("BufWr",[3,0,1,2])
    ,("Drand",[1,0])
    ,("Dseq",[1,0])
    ,("Dser",[1,0])
    ,("Dswitch",[1,0])
    ,("Dswitch1",[1,0])
    ,("Duty",[0,1,3,2])
    ,("Dxrand",[1,0])
    ,("EnvGen",[5,0,1,2,3,4])
    ,("Klang",[2,0,1])
    ,("Klank",[4,0,1,2,3])
    ,("LocalBuf",[1,0])
    ,("PackFFT",[0,1,6,2,3,4]) -- 5: implicit
    ,("RecordBuf",[8,0,1,2,3,4,5,6,7])
    ,("SetBuf",[0,2,1,3]) -- 3: missing (complicated case...)
    ,("TDuty",[0,1,3,2,4])
    ,("TWindex",[0,2,1])]

-- | Indices are /UGen/ indices, not /sclang/ indices.
meta_flattens_mce :: [(String,Int)]
meta_flattens_mce =
    [("BufWr",3)
    ,("DiskOut",1)
    ,("Drand",1)
    ,("Dseq",1)
    ,("Dser",1)
    ,("Dswitch",1)
    ,("Dswitch1",1)
    ,("Dxrand",1)
    ,("EnvGen",5)
    ,("Klang",2)
    ,("Klank",4)
    ,("LocalIn",0) -- defaults
    ,("LocalOut",0)
    ,("OffsetOut",1)
    ,("Out",1)
    ,("PackFFT",5)
    ,("RecordBuf",8)
    ,("ReplaceOut",1)
    ,("SetBuf",3)
    ,("Select",1)
    ,("TWindex",2)
    ,("XOut",2)]

-- | Indices are /UGen/ indices, not /sclang/ indices.
meta_enumeration_inputs :: [(String,[(Int,String)])]
meta_enumeration_inputs =
    [("BufRd",[(2,"Loop"),(3,"Interpolation")])
    ,("BufWr",[(2,"Loop")])
    ,("DetectSilence",[(3,"DoneAction")])
    ,("DiskIn",[(1,"Loop")])
    ,("Dbufrd",[(2,"Loop")])
    ,("Dbufwr",[(3,"Loop")])
    ,("DemandEnvGen",[(9,"DoneAction")])
    ,("Duty",[(3,"DoneAction")])
    ,("EnvGen",[(4,"DoneAction")])
    ,("LFGauss",[(3,"Loop"),(4,"DoneAction")])
    ,("Line",[(3,"DoneAction")])
    ,("Linen",[(4,"DoneAction")])
    ,("XLine",[(3,"DoneAction")])
    ,("PlayBuf",[(4,"Loop"),(5,"DoneAction")])
    ,("RecordBuf",[(5,"Loop"),(7,"DoneAction")])
    ,("TDuty",[(2,"DoneAction")])
    ,("VDiskIn",[(2,"Loop")])]

-- | Indices are /UGen/ indices, not /sclang/ indices.
meta_filters :: [(String,[Int])]
meta_filters =
    [("AllpassC",[0])
    ,("AllpassL",[0])
    ,("AllpassN",[0])
    ,("BAllPass",[0])
    ,("BBandPass",[0])
    ,("BBandStop",[0])
    ,("BHiPass",[0])
    ,("BHiShelf",[0])
    ,("BLowPass",[0])
    ,("BLowShelf",[0])
    ,("BPF",[0])
    ,("BPZ2",[0])
    ,("BPeakEQ",[0])
    ,("BRF",[0])
    ,("BRZ2",[0])
    ,("BinaryOpUGen",[0,1])
    ,("CheckBadValues",[0])
    ,("Clip",[0])
    ,("CoinGate",[1])
    ,("CombC",[0])
    ,("CombL",[0])
    ,("CombN",[0])
    ,("Compander",[0])
    ,("Decay",[0])
    ,("Decay2",[0])
    ,("DegreeToKey",[1])
    ,("Delay1",[0])
    ,("Delay2",[0])
    ,("DelayC",[0])
    ,("DelayL",[0])
    ,("DelayN",[0])
    ,("Demand",[0])
    ,("ExpRand",[0,1])
    ,("FOS",[0])
    ,("Fold",[0])
    ,("Formlet",[0])
    ,("FreeVerb",[0])
    ,("FreeVerb2",[0])
    ,("GVerb",[0])
    ,("Gate",[0])
    ,("HPF",[0])
    ,("HPZ1",[0])
    ,("HPZ2",[0])
    ,("Hasher",[0])
    ,("Hilbert",[0])
    ,("IRand",[0,1])
    ,("InRange",[0])
    ,("Integrator",[0])
    ,("Klank",[0])
    ,("LPF",[0])
    ,("LPZ1",[0])
    ,("LPZ2",[0])
    ,("Lag",[0])
    ,("Lag2",[0])
    ,("Lag2UD",[0])
    ,("Lag3",[0])
    ,("Lag3UD",[0])
    ,("LagUD",[0])
    ,("LastValue",[0])
    ,("Latch",[0])
    ,("LeakDC",[0])
    ,("Limiter",[0])
    ,("LinExp",[0])
    ,("LinPan2",[0])
    ,("LinRand",[0,1])
    ,("LocalOut",[0])
    ,("MantissaMask",[0])
    ,("Median",[1])
    ,("MidEQ",[0])
    ,("MoogFF",[0])
    ,("MostChange",[0,1])
    ,("MulAdd",[0])
    ,("NRand",[0,1,2])
    ,("Normalizer",[0])
    ,("OnePole",[0])
    ,("OneZero",[0])
    ,("Out",[1])
    ,("Pan2",[0])
    ,("Peak",[0])
    ,("PeakFollower",[0])
    ,("PitchShift",[0])
    ,("Pluck",[0])
    ,("Poll",[1])
    ,("PulseCount",[0])
    ,("PulseDivider",[0])
    ,("RHPF",[0])
    ,("RLPF",[0])
    ,("Ramp",[0])
    ,("Rand",[0,1])
    ,("ReplaceOut",[1])
    ,("Resonz",[0])
    ,("Ringz",[0])
    ,("Rotate2",[0,1])
    ,("RunningMax",[0])
    ,("RunningMin",[0])
    ,("RunningSum",[0])
    ,("SOS",[0])
    ,("Select",[0,1])
    ,("SendReply",[0])
    ,("SendTrig",[0])
    ,("SetResetFF",[0])
    ,("Shaper",[1])
    ,("Slew",[0])
    ,("Slope",[0])
    ,("Stepper",[0])
    ,("Sum3",[0,1,2])
    ,("Sum4",[0,1,2,3])
    ,("Sweep",[0])
    ,("TDelay",[0])
    ,("TExpRand",[2])
    ,("TIRand",[2])
    ,("TRand",[2])
    ,("Timer",[0])
    ,("ToggleFF",[0])
    ,("Trig",[0])
    ,("Trig1",[0])
    ,("TWindex",[0])
    ,("TwoPole",[0])
    ,("TwoZero",[0])
    ,("UnaryOpUGen",[0])
    ,("Wrap",[0])
    ,("WrapIndex",[1])
    ,("XOut",[2])
    -- SC3 Plugins
    ,("ComplexRes",[0])
    ,("JPverbRaw",[0])
    ,("DiodeRingMod",[0])
    ,("Greyhole",[0])]

-- | Some of the fixed rate UGens are also at filter...
meta_fixed_rate :: [(String,Rate)]
meta_fixed_rate =
    [("ControlDur",IR)
    ,("ControlRate",IR)
    ,("ExpRand",IR)
    ,("GrainBuf",AR)
    ,("GrainFM",AR)
    ,("GrainIn",AR)
    ,("GrainSin",AR)
    ,("IRand",IR)
    ,("K2A",AR)
    ,("LinRand",IR)
    ,("LocalBuf",IR)
    ,("NRand",IR)
    ,("NumAudioBuses",IR)
    ,("NumBuffers",IR)
    ,("NumControlBuses",IR)
    ,("NumInputBuses",IR)
    ,("NumOutputBuses",IR)
    ,("NumRunningSynths",IR)
    ,("RadiansPerSample",IR)
    ,("Rand",IR)
    ,("SampleDur",IR)
    ,("SampleRate",IR)
    ,("SetBuf",IR)
    ,("SubsampleOffset",IR)
    ,("Warp1",AR)]

-- | nondet = non-deterministic.
meta_nondet :: [String]
meta_nondet =
    ["BrownNoise"
    ,"ClipNoise"
    ,"CoinGate"
    ,"Drand"
    ,"Dseq"
    ,"Dshuf"
    ,"Dust"
    ,"Dust2"
    ,"Dwrand"
    ,"Dxrand"
    ,"ExpRand"
    ,"Gendy1"
    ,"Gendy2"
    ,"Gendy3"
    ,"GrayNoise"
    ,"IRand"
    ,"LFClipNoise"
    ,"LFDClipNoise"
    ,"LFDNoise0"
    ,"LFDNoise1"
    ,"LFDNoise3"
    ,"LFNoise0"
    ,"LFNoise1"
    ,"LFNoise1"
    ,"LFNoise2"
    ,"LinRand"
    ,"LocalBuf"
    ,"NRand"
    ,"PinkNoise"
    ,"Rand"
    ,"TChoose"
    ,"TExpRand"
    ,"TIRand"
    ,"TRand"
    ,"TWChoose"
    ,"TWindex"
    ,"Vibrato"
    ,"WhiteNoise"
    ]

-- | This is unnecessary, it's generated from 'u_is_demand_rate'.
meta_demand :: [String]
meta_demand = ["Dbrown","Dbufrd","Dbufwr","Dgeom","Dibrown","Diwhite","Donce","Dpoll","Drand","Dreset","Dseq","Dser","Dseries","Dshuf","Dstutter","Dswitch","Dswitch1","Dunique","Dwhite","Dwrand","Dxrand"]

-- * Predicates

ugen_is_filter :: String -> Bool
ugen_is_filter = isJust . flip lookup meta_filters

ugen_is_operator :: String -> Bool
ugen_is_operator = flip elem ["UnaryOpUGen","BinaryOpUGen"]
