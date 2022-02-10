-- sawed cymbals (jmcc) #9 ; texture=overlap,4,4,6,inf
let y _ = let f1 = rand 500 2500
              f = mceFill 15 (\_ -> rand f1 (f1 + rand 0 8000))
          in klankSpec_mce f (mceConst 15 1) (X.randN 15 2 6)
    fS = xLine kr (rand 0 600) (rand 0 600) 12 DoNothing
in klank (lfSaw ar fS 0 * 0.0005) 1 0 1 (mceTranspose (mceFill 2 y))

-- sawed cymbals (jmcc) #9 ; texture=overlap,4,4,6,inf ; id
let y z _ = let f1 = randId (z,'α') 500 2500
                f = listFillId 'γ' 15 (\z' _ -> randId (z','δ') f1 (f1 + randId (z','β') 0 8000))
                rt = listFillId 'ε' 15 (\z' _ -> randId (z','ζ') 2 6)
            in klankSpec f (replicate 15 1) rt
    fS = xLine kr (randId 'θ' 0 600) (randId 'ι' 0 600) 12 DoNothing
in klank (lfSaw ar fS 0 * 0.0005) 1 0 1 (mceTranspose (mceFillId 'η' 2 y))
