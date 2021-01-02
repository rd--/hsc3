-- sawed cymbals (jmcc) #9 ; texture=overlap,4,4,6,inf
let y = let f1 = rand 'α' 500 2500
            f2 = rand 'β' 0 8000
            f = Protect.uclone_seq (const False) 'γ' 15 (rand 'δ' f1 (f1 + f2))
            rt = Protect.uclone_seq (const False) 'ε' 15 (rand 'ζ' 2 6)
        in klankSpec f (replicate 15 1) rt
    z = Protect.uclone_all 'η' 2 y
    fS = xLine KR (rand 'θ' 0 600) (rand 'ι' 0 600) 12 DoNothing
in klank (lfSaw AR fS 0 * 0.0005) 1 0 1 (mceTranspose z)
