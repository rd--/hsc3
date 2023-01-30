-- stkInst ; bowed ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Bowed.html
let f (_,g,_,y,z,o,rx,_,p,_,_) =
      let kBowPressure = 2
          kBowPosition = 4
          kVibratoFrequency = 11
          kVibratoGain = 1
          kVolume = 128
          args = [kBowPressure,1 + z * 127
                 ,kBowPosition,1 + y * 127
                 ,kVibratoFrequency,50
                 ,kVibratoGain,1
                 ,kVolume,1 + z * 127]
          s = X.stkInst ar (unitCps p) g (1 - rx) 0.5 (X.stkAt "Bowed") (mce args)
      in pan2 s (o * 2 - 1) g
in mix (voicer 16 f) * control kr "gain" 1

-- stkInst ; brass ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Brass.html
let f (_,g,_,y,z,o,rx,ry,p,_,_) =
      let kLipTension = 2
          kSlideLength = 4
          kVibratoFrequency = 11
          kVibratoGain = 1
          kVolume = 128
          args = [kLipTension,latch rx g * 127
                 ,kSlideLength,latch y g * 127
                 ,kVibratoFrequency,ry * 48
                 ,kVibratoGain,1
                 ,kVolume,6 + z * 24]
          s = X.stkInst ar (unitCps p) g (1 - rx) 0.5 (X.stkAt "Brass") (mce args)
      in pan2 s (o * 2 - 1) g
in mix (voicer 16 f) * control kr "gain" 2

-- stkInst ; clarinet ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Clarinet.html
let f (_,g,_,y,z,o,rx,ry,p,_,_) =
      let kReedStiffness = 2
          kNoiseGain = 4
          kVibratoFrequency = 11
          kVibratoGain = 1
          kBreathPressure = 128
          args = [kReedStiffness,y * 32
                 ,kNoiseGain,rx * 32
                 ,kVibratoFrequency,50
                 ,kVibratoGain,1
                 ,kBreathPressure,linLin z 0 1 48 64]
          s = X.stkInst ar (unitCps p) g (1 - ry) 0.5 (X.stkAt "Clarinet") (mce args)
      in pan2 s (o * 2 - 1) g
in mix (voicer 16 f) * control kr "gain" 1

-- stkInst ; flute ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Flute.html
let f (_,g,_,y,z,o,rx,ry,p,_,_) =
      let kJetDelay = 2
          kNoiseGain = 4
          kVibratoFrequency = 11
          kVibratoGain = 1
          kBreathPressure = 128
          args = [kJetDelay,y * 128
                 ,kNoiseGain,rx * 64
                 ,kVibratoFrequency,(1 - ry) * 64
                 ,kVibratoGain,ry * 16
                 ,kBreathPressure,linLin z 0 1 48 96]
          s = X.stkInst ar (unitCps p) g 0.75 0.5 (X.stkAt "Flute") (mce args)
      in pan2 s (o * 2 - 1) g
in mix (voicer 16 f) * control kr "gain" 0.5

-- stkInst ; mandolin ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Mandolin.html
let f (_,g,_,y,z,o,rx,ry,p,_,_) =
      let kBodySize = 2
          kPluckPosition = 4
          kStringDetuning = 1
          args = [kBodySize,ry * 127
                 ,kPluckPosition,y * 127
                 ,kStringDetuning,rx * 127]
          s = X.stkInst ar (unitCps p * 1.5) g (z * 2) 0.5 (X.stkAt "Mandolin") (mce args)
      in pan2 s (o * 2 - 1) (lagUD g 0 5)
in mix (voicer 16 f) * control kr "gain" 2

-- stkInst ; mesh2D ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Mesh2D.html
let f (_,g,x,y,z,o,rx,ry,p,_,_) =
      let kXDimension = 2
          kYDimension = 4
          kMeshDecay = 11
          kXYInputPosition = 1
          args = [kXDimension,latch x g * 127
                 ,kYDimension,latch y g * 127
                 ,kMeshDecay,(1 - z) * 127
                 ,kXYInputPosition,rx * 127]
          s = X.stkInst ar (unitCps p) g (z * 2) 0 (X.stkAt "Mesh2D") (mce args)
      in pan2 s (o * 2 - 1) 1
in mix (voicer 16 f) * control kr "gain" 2

-- stkInst ; plucked ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Plucked.html
let f (_,g,_,y,z,o,_,_,p,_,_) =
      let args = [0] -- cannot be []...
          s = X.stkInst ar (unitCps p) g (0.5 + z * 0.5) (y * 0.01) (X.stkAt "Plucked") (mce args)
      in pan2 s (o * 2 - 1) 1
in mix (voicer 16 f) * control kr "gain" 1

-- stkInst ; sitar ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Sitar.html
let f (_,g,_,y,z,o,_,_,p,_,_) =
      let args = [0] -- cannot be []...
          s = X.stkInst ar (unitCps (latch p g)) g (0.5 + z * 0.5) (y * 0.01) (X.stkAt "Sitar") (mce args)
      in pan2 s (o * 2 - 1) 1
in mix (voicer 16 f) * control kr "gain" 1

-- stkInst ; voicForm ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1VoicForm.html
let f (_,g,_,y,z,o,rx,ry,p,_,_) =
      let kVoicedUnvoicedMix = 2
          kVowelPhonemeSelection = 4
          kVibratoFrequency = 11
          kVibratoGain = 1
          kLoudness = 128
          args = [kVoicedUnvoicedMix,lag rx 0.2 * 127
                 ,kVowelPhonemeSelection,lag y 0.2 * 16]
          s = X.stkInst ar (unitCps p) g 1 0 (X.stkAt "VoicForm") (mce args)
      in pan2 s (o * 2 - 1) (z * lagUD g 0.1 2)
in mix (voicer 16 f) * control kr "gain" 1

---- ; set globals
X.stkGlobals ar 1 1 (label "/home/rohan/opt/src/supercollider/sc3-plugins/external_libraries/stk/rawwaves")
