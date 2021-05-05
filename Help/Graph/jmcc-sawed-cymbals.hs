-- sawed cymbals (jmcc) #9 ; texture=overlap,4,4,6,inf
let y z _ = let f1 = rand (z,'α') 500 2500
                f2 = rand (z,'β') 0 8000
                f = listFill_z 'γ' 15 (\z _ -> rand (z,'δ') f1 (f1 + f2))
                rt = listFill_z 'ε' 15 (\z _ -> rand (z,'ζ') 2 6)
            in klankSpec f (replicate 15 1) rt
    z = mceFill_z 'η' 2 y
    fS = xLine KR (rand 'θ' 0 600) (rand 'ι' 0 600) 12 DoNothing
in klank (lfSaw AR fS 0 * 0.0005) 1 0 1 (mceTranspose z)
