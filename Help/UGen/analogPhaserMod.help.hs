-- analogPhaserMod
let amp = 0.25
    osc = sinOsc AR (lfNoise2 'α' AR 10 * 1000) 0
    flt = X.analogPhaserMod osc 0 1.0 50
    drywet = lfSaw AR 0.1 0
    sig = xFade2 (sinOsc AR (osc * 401.0) 0) (sinOsc AR (flt * 401.0) 0) drywet amp
in pan2 sig 0 1
