-- http://sccode.org/1-V (nv) L1
let a = mce2 (pinkNoise 'α' AR) (pinkNoise 'β' AR)
    nd z i =
        let n = lfNoise1 (z,'γ') KR (rand (z,'δ') 0 0.05)
            f = linExp n (-1) 1 40 15000
        in bBandStop i f (expRand (z,'ε') 0.1 2)
in lpf (useq_z 'ζ' 50 nd a) 100000
