-- MiRipples ; basic
X.miRipples (saw AR 60) 0.4 0.3 1

-- MiRipples
let cf = range 0 1 (lfNoise0 'α' KR 4)
in X.miRipples (saw AR 60) cf 0.6 1 * 0.5

-- MiRipples ; filter sweep + distortion
let cf = range 0.1 0.9 (lfTri KR 0.03 0)
in X.miRipples (saw AR 40) cf 0.8 4 * 0.2

-- MiRipples
let tr = impulse AR 4 0
    input = decay tr 0.1
    cf = tRand 'α' 0.1 0.6 tr
    filt = rlpf cf 80 0.3
in X.miRipples input filt 0.8 3 * 0.5
