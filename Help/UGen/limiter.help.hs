-- limiter
let z = let i = impulse ar 8 0 * lfSaw kr 0.25 0 * (-0.6) + 0.7
        in decay2 i 0.001 0.3 * fSinOsc ar 500 0
in mce2 (z * 0.0001) (limiter z 0.4 0.01 * 0.1)
