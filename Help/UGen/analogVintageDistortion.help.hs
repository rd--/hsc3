-- analogVintageDistortion
let amp = control kr "amp" 0.025
    drivegain = control kr "drivegain" 0.85
    bias = control kr "bias" 0.1
    lowgain = control kr "lowgain" (dbAmp (-3))
    highgain = control kr "highgain" (dbAmp (-9))
    shelvingfreq = control_m kr "shelvingfreq" 600 (200,800,"exp")
    oversample = control_m kr "oversample" 1 (0,1,"switch")
    sig = sinOsc ar (sinOsc kr 1 0 `in_exprange` (80,1000)) 0
    flt = X.analogVintageDistortion sig drivegain bias lowgain highgain shelvingfreq oversample
in mce2 (sig * 0.5) flt * amp

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 0 "audio" 0
