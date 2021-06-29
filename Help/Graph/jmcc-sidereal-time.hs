-- sidereal time (jmcc) #9 ; texture=overlap,4,4,6,inf
let p = 15
    i = let y z _ = let fr = listFill_z 'α' p (\e _ -> expRand (z,e,'β') 100 6000)
                        rt = listFill_z 'γ' p (\e _ -> rand (z,e,'δ') 2 6)
                    in klankSpec fr (replicate p 1) rt
        in mceFill_z 'ε' 2 y
    f = xLine kr (expRand 'ζ' 40 300) (expRand 'η' 40 300) 12 DoNothing
    t = let e = lfNoise2 'θ' kr (rand 'ι' 0 8)
        in lfPulse ar f 0 (rand 'κ' 0.1 0.9) * 0.002 * max 0 e
    o = distort (klank t 1 0 1 (mceTranspose i)) * 0.1
in combN o 0.6 (rand 'λ' 0.1 0.6) 8 + mceReverse o
