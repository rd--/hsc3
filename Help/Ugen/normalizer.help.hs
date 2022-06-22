-- normalizer
let s = fSinOsc ar 500 0
    t = impulse ar 8 (lfSaw kr 0.25 (-0.6) * 0.7)
    z = decay2 t 0.001 0.3 * s
in mce [z, normalizer z 0.4 0.01]
