-- analogPhaserMod
let amp = 0.25
    osc = sinOsc ar (lfNoise2Id 'α' ar 10 * 1000) 0
    flt = X.analogPhaserMod osc 0 1.0 50
    drywet = lfSaw ar 0.1 0
    sig = xFade2 (sinOsc ar (osc * 401.0) 0) (sinOsc ar (flt * 401.0) 0) drywet amp
in pan2 sig 0 1
