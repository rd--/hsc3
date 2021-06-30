-- impulse sequencer (jmcc) SC2
let dsequ s tr = demand tr 0 (dseq dinf (mce s))
    t = impulse ar 8 0 {- single sample impulse as trigger -}
    c_sq = dsequ [1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0] t * t {- clave rhythm -}
    c = decay2 c_sq 0.001 0.3 * fSinOsc ar 1700 0 * 0.1
    d_sq = dsequ [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0] t * t
    d = decay2 d_sq 0.001 0.3 * fSinOsc ar 2400 0 * 0.04
    n_sq = dsequ [1.0, 0.1, 0.1, 1.0, 0.1, 1.0, 0.1, 0.1] t * t {- noise drum -}
    n = decay2 n_sq 0.001 0.25 * brownNoise ar * 0.1
    b_sq = dsequ [1,0,0.2,0,0.2,0,0.2,0] t * t {- bass drum -}
    b = decay2 b_sq 0.001 0.5 * fSinOsc ar 100 0 * 0.2
in c + d + n + b

-- impulse sequencer (jmcc) SC2 ; id
let dsequId z s tr = demand tr 0 (dseqId z dinf (mce s))
    t = impulse ar 8 0 {- single sample impulse as trigger -}
    c_sq = dsequId 'α' [1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0] t * t {- clave rhythm -}
    c = decay2 c_sq 0.001 0.3 * fSinOsc ar 1700 0 * 0.1
    d_sq = dsequId 'β' [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0] t * t
    d = decay2 d_sq 0.001 0.3 * fSinOsc ar 2400 0 * 0.04
    n_sq = dsequId 'γ' [1.0, 0.1, 0.1, 1.0, 0.1, 1.0, 0.1, 0.1] t * t {- noise drum -}
    n = decay2 n_sq 0.001 0.25 * brownNoiseId 'δ' ar * 0.1
    b_sq = dsequId 'ε' [1,0,0.2,0,0.2,0,0.2,0] t * t {- bass drum -}
    b = decay2 b_sq 0.001 0.5 * fSinOsc ar 100 0 * 0.2
in c + d + n + b
