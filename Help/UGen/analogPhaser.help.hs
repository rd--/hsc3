-- analogPhaser
let drywet = 0
    amp = 0.01
    pan = 0
    osc = X.varShapeOsc AR 110 0.5 (sinOsc KR 0.0915 0 `in_range` (0,1)) 1 151
    skew = sinOsc KR 0.059 0
    feedback = sinOsc KR 0.005 pi `in_range` (0,0.85)
    modulation = sinOsc KR 0.0192 two_pi `in_range` (0,1)
    stages = 50
    flt = X.analogPhaser osc (sinOsc AR 0.22 0) skew feedback modulation stages
in pan2 (xFade2 osc flt drywet 1) pan amp

-- analogPhaser ; controls
let k = control KR
    drywet = k "drywet" 0
    amp = k "amp" 0.01
    pan = k "pan" 0
    freq = k "freq" 110
    osc = X.varShapeOsc AR freq 0.5 (sinOsc KR 0.0915 0 `in_range` (0,1)) 1 151
    skew = sinOsc KR 0.059 0
    feedback = sinOsc KR 0.005 pi `in_range` (0,0.85)
    modulation = sinOsc KR 0.0192 two_pi `in_range` (0,1)
    stages = control_m KR "stage" 50 (8,50,"lin")
    flt = X.analogPhaser osc (sinOsc AR 0.22 0) skew feedback modulation stages
in pan2 (xFade2 osc flt drywet 1) pan amp
