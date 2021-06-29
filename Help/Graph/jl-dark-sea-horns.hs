-- http://sccode.org/1-j#c51 (jl)
let n = lfNoise1
    x = localIn' 2 ar
    a = tanh (sinOsc ar 65 (x * lfNoise1 ar 0.1 * 3) * lfNoise1 ar 3 * 6)
    f i = allpassN i 0.3 (X.rRandN 2 0.1 0.3) 5
    o = tanh (iter 9 f a)
in mrg2 (o * 0.2) (localOut o)

-- http://sccode.org/1-j#c51 (jl) ; id
let n = lfNoise1Id
    x = localIn' 2 ar
    a = tanh (sinOsc ar 65 (x * n 'α' ar 0.1 * 3) * n 'β' ar 3 * 6)
    f z i = allpassN i 0.3 (X.rRandNId 2 (z,'γ') 0.1 0.3) 5
    o = tanh (useq_z 'δ' 9 f a)
in mrg2 (o * 0.2) (localOut o)
