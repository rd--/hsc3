-- http://sccode.org/1-j#c51 (jl)
let n = lfNoise1
    x = localIn' 2 AR
    a = tanh (sinOsc AR 65 (x * n 'α' AR 0.1 * 3) * n 'β' AR 3 * 6)
    f z i = allpassN i 0.3 (X.rRandN 2 (z,'γ') 0.1 0.3) 5
    o = tanh (useq_z 'δ' 9 f a)
in mrg2 (o * 0.2) (localOut o)
