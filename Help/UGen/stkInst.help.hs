-- stkInst ; mandolin ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Mandolin.html
let f _ (g,_,y,z,o,rx,ry,p) =
      let kBodySize = 2
          kPluckPosition = 4
          kStringDetuning = 1
          args = [kBodySize,ry * 127,kPluckPosition,y * 127,kStringDetuning,rx * 127]
          s = X.stkInst AR (midiCPS p * 2) g z 0.5 (X.stkAt "Mandolin") (mce args)
      in pan2 s (o * 2 - 1) (lagUD g 0 5)
in mix (rEventVoicer 16 f) * control KR "gain" 2

-- stkInst ; bowed ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Bowed.html
let f _ (g,_,y,z,o,_,_,p) =
      let kBowPressure = 2
          kBowPosition = 4
          kVibratoFrequency = 11
          kVibratoGain = 1
          kVolume = 128
          args = [kBowPressure,z * 127,kBowPosition,y * 127,kVibratoFrequency,50,kVibratoGain,1,kVolume,z * 127]
          s = X.stkInst AR (midiCPS p) g 1 0.5 (X.stkAt "Bowed") (mce args)
      in pan2 s (o * 2 - 1) g
in mix (rEventVoicer 16 f) * control KR "gain" 1

-- stkInst ; clarinet ; event control ; https://ccrma.stanford.edu/software/stk/classstk_1_1Clarinet.html
let f _ (g,_,y,z,o,rx,_,p) =
      let kReedStiffness = 2
          kNoiseGain = 4
          kVibratoFrequency = 11
          kVibratoGain = 1
          kBreathPressure = 128
          args = [kReedStiffness,y * 32,kNoiseGain,rx * 32,kVibratoFrequency,50,kVibratoGain,1,kBreathPressure,linLin z 0 1 48 64]
          s = X.stkInst AR (midiCPS p) g 1 0.5 (X.stkAt "Clarinet") (mce args)
      in pan2 s (o * 2 - 1) g
in mix (rEventVoicer 16 f) * control KR "gain" 1

---- ; set globals
X.stkGlobals AR 1 1 (label "/home/rohan/opt/src/sc3-plugins/external_libraries/stk/rawwaves")
