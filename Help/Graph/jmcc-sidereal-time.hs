-- sidereal time (jmcc) #9 ; texture=overlap,4,4,6,inf
let p = 15
    z = let y = let fr = Protect.uclone_seq (const False) 'α' p (expRand 'β' 100 6000)
                    rt = Protect.uclone_seq (const False) 'γ' p (rand 'δ' 2 6)
                in klankSpec fr (replicate p 1) rt
        in Protect.uclone_all 'ε' 2 y
    f = xLine KR (expRand 'ζ' 40 300) (expRand 'η' 40 300) 12 DoNothing
    t = let e = lfNoise2 'θ' KR (rand 'ι' 0 8)
        in lfPulse AR f 0 (rand 'κ' 0.1 0.9) * 0.002 * max 0 e
    o = distort (klank t 1 0 1 (mceTranspose z)) * 0.1
in combN o 0.6 (rand 'λ' 0.1 0.6) 8 + mceReverse o
