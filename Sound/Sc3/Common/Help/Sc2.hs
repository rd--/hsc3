-- | Functions related to the Sc2 help files.
module Sound.Sc3.Common.Help.Sc2 where

{- | Approximate Ugen categorisation from Sc2.
The help files were organised in sub-directories.

> map fst sc2_ugen_categories
-}
sc2_ugen_categories :: [(String, [String])]
sc2_ugen_categories =
  [
    ( "Analysis"
    ,
      [ "Pitch"
      , "RunningSum"
      , "Slope"
      , "ZeroCrossing"
      ]
    )
  ,
    ( "BinaryOps"
    , []
    )
  ,
    ( "Buffers"
    , ["TGrains"]
    )
  ,
    ( "Controls"
    ,
      [ "DegreeToKey"
      , "GetTempo"
      , "Impulse"
      , "Index"
      , "Integrator"
      , "K2A"
      , "Lag"
      , "Latch"
      , "LFPulse"
      , "LFSaw"
      , "LFTri"
      , "LinExp"
      , "MouseX"
      , "MouseY"
      , "Osc1"
      , "Slew"
      , "WrapIndex"
      ]
    )
  ,
    ( "Delays"
    ,
      [ "AllpassC"
      , "CombC"
      , "Delay1"
      , "Delay2"
      , "DelayC"
      , "DelayWrite"
      , "MultiTapDelay"
      , "PingPongDelay"
      , "PitchShift"
      ]
    )
  ,
    ( "Envelopes"
    ,
      [ "Cutoff"
      , "Decay2"
      , "Decay"
      , "Ln"
      , "XLn"
      ]
    )
  ,
    ( "Events"
    , ["OverlapTexture"]
    )
  ,
    ( "Filters"
    ,
      [ "BPF"
      , "BPZ2"
      , "BRF"
      , "BRZ2"
      , "Formlet"
      , "FOS"
      , "HPF"
      , "HPZ1"
      , "HPZ2"
      , "LeakDC"
      , "Limiter"
      , "LPF"
      , "LPZ1"
      , "LPZ2"
      , "Median"
      , "Normalizer"
      , "OnePole"
      , "OneZero"
      , "Resonz"
      , "RHPF"
      , "RingzBank"
      , "Ringz"
      , "RLPF"
      , "SOS"
      , "TwoPole"
      , "TwoZero"
      ]
    )
  ,
    ( "Misc"
    , ["Mix"]
    )
  ,
    ( "Noise"
    ,
      [ "BrownNoise"
      , "ClipNoise"
      , "Crackle"
      , "Dust2"
      , "Dust"
      , "GrayNoise"
      , "LatoocarfianC"
      , "LFClipNoise"
      , "LFNoise0"
      , "LFNoise1"
      , "LFNoise2"
      , "LinCongC"
      , "PinkNoise"
      , "WhiteNoise"
      ]
    )
  ,
    ( "Oscillators"
    ,
      [ "Blip"
      , "Formant"
      , "FSinOsc"
      , "Impulse"
      , "LFPulse"
      , "LFSaw"
      , "LFTri"
      , "PMOsc"
      , "Pulse"
      , "Saw"
      , "SinOsc"
      , "SyncSaw"
      , "VarSaw"
      ]
    )
  ,
    ( "Panners"
    ,
      [ "LinPan2"
      , "LinXFade2"
      , "Pan2"
      , "PanAz"
      , "PanB"
      ]
    )
  ,
    ( "Random"
    ,
      [ "CoinGate"
      , "ExpRand"
      , "IRand"
      , "LinRand"
      , "NRand"
      , "Rand"
      , "TExpRand"
      , "TIRand"
      , "TRand"
      ]
    )
  ,
    ( "Samples"
    ,
      [ "AudioIn"
      , "BufRd"
      , "PlayBuf"
      ]
    )
  ,
    ( "Triggers"
    ,
      [ "Gate"
      , "InRange"
      , "PeakFollower"
      , "Phasor"
      , "PulseCount"
      , "PulseDivider"
      , "Schmidt"
      , "Stepper"
      , "Sweep"
      , "TDelay"
      , "Timer"
      , "ToggleFF"
      ]
    )
  ,
    ( "UnaryOperators"
    ,
      [ "abs"
      , "acos"
      , "ampdb"
      , "asin"
      , "atan"
      , "ceil"
      , "cos"
      , "cosh"
      , "cpsmidi"
      , "cpsoct"
      , "cubed"
      , "dbamp"
      , "distort"
      , "exp"
      , "floor"
      , "frac"
      , "isNegative"
      , "isPositive"
      , "isStrictlyPositive"
      , "log10"
      , "log2"
      , "log"
      , "midicps"
      , "neg"
      , "octcps"
      , "reciprocal"
      , "sign"
      , "sin"
      , "sinh"
      , "softclip"
      , "sqrt"
      , "squared"
      , "tan"
      , "tanh"
      ]
    )
  ]
