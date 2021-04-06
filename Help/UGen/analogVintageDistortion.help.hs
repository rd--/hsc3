-- analogVintageDistortion
let amp = control KR "amp" 0.025
    drivegain = control KR "drivegain" 0.85
    bias = control KR "bias" 0.1
    lowgain = control KR "lowgain" (dbAmp (-3))
    highgain = control KR "highgain" (dbAmp (-9))
    shelvingfreq = control_m KR "shelvingfreq" 600 (200,800,"exp")
    oversample = control_m KR "oversample" 1 (0,1,"switch")
    sig = sinOsc AR (sinOsc KR 1 0 `in_exprange` (80,1000)) 0
    flt = X.analogVintageDistortion sig drivegain bias lowgain highgain shelvingfreq oversample
in mce2 (sig * 0.5) flt * amp

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 0
