-- sidereal time (jmcc) #9 ; texture=overlap,4,4,6,inf
let p = 15
    i = let y _ = klankSpec_mce (X.rExpRandN p 100 6000) (mceConst p 1) (X.rRandN p 2 6)
        in mceFill 2 y
    f = xLine kr (expRand 40 300) (expRand 40 300) 12 DoNothing
    t = let e = lfNoise2 kr (rand 0 8)
        in lfPulse ar f 0 (rand 0.1 0.9) * 0.002 * max 0 e
    o = distort (klank t 1 0 1 (mceTranspose i)) * 0.1
in combN o 0.6 (rand 0.1 0.6) 8 + mceReverse o

-- sidereal time (jmcc) #9 ; texture=overlap,4,4,6,inf ; id
let p = 15
    i = let y z _ = let fr = X.rExpRandNId p (z,'β') 100 6000
                        rt = X.rRandNId p (z,'δ') 2 6
                    in klankSpec_mce fr (mceConst p 1) rt
        in mceFill_z 'ε' 2 y
    f = xLine kr (expRandId 'ζ' 40 300) (expRandId 'η' 40 300) 12 DoNothing
    t = let e = lfNoise2Id 'θ' kr (randId 'ι' 0 8)
        in lfPulse ar f 0 (randId 'κ' 0.1 0.9) * 0.002 * max 0 e
    o = distort (klank t 1 0 1 (mceTranspose i)) * 0.1
in combN o 0.6 (randId 'λ' 0.1 0.6) 8 + mceReverse o
