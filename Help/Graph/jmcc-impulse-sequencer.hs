-- impulse sequencer (jmcc) SC2
let dsequ z s tr = demand tr 0 (dseq z dinf (mce s))
    t = impulse AR 8 0 {- single sample impulse as trigger -}
    c_sq = dsequ 'α' [1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0] t * t {- clave rhythm -}
    c = decay2 c_sq 0.001 0.3 * fSinOsc AR 1700 0 * 0.1
    d_sq = dsequ 'β' [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0] t * t
    d = decay2 d_sq 0.001 0.3 * fSinOsc AR 2400 0 * 0.04
    n_sq = dsequ 'γ' [1.0, 0.1, 0.1, 1.0, 0.1, 1.0, 0.1, 0.1] t * t {- noise drum -}
    n = decay2 n_sq 0.001 0.25 * brownNoise 'δ' AR * 0.1
    b_sq = dsequ 'ε' [1,0,0.2,0,0.2,0,0.2,0] t * t {- bass drum -}
    b = decay2 b_sq 0.001 0.5 * fSinOsc AR 100 0 * 0.2
in c + d + n + b
