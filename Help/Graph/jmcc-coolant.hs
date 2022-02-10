-- coolant (jmcc) #2 ; texture=overlap,4,4,2,inf
let p = 10
    spec freq = let n = mceFill p (const 1) in klankSpec_mce freq n n
    gen _ = klank (onePole (brownNoise ar * 0.0015) 0.95) 1 0 1 (spec (X.randN p 40 2400))
in mceFill 2 gen

-- coolant (jmcc) #2 ; texture=overlap,4,4,2,inf ; id
let p = 10
    s = onePole (mceFillId 'α' 2 (\z _ -> brownNoiseId z ar * 0.0015)) 0.95
    n = mceFill p (const 1)
    s1 = klankSpec_mce (X.randNId p 'β' 40 2400) n n
    s2 = klankSpec_mce (X.randNId p 'γ' 40 2400) n n
in klank s 1 0 1 (mceTranspose (mce2 s1 s2))
