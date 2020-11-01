-- tiRand
let l = tiRand 'α' (-1) 1 (dust 'β' KR 10)
in pan2 (pinkNoise 'γ' AR * 0.1) l 1

-- tiRand ; monadic variant
uid_st_eval
 (do l <- tiRandM (-1) 1 =<< dustM KR 10
     n <- pinkNoiseM AR
     return (pan2 (n * 0.1) l 1))

-- tiRand ; osc frequency
uid_st_eval
 (do n <- tiRandM 4 12 =<< dustM KR 10
     let f = n * 150 + (mce [0,1])
     return (sinOsc AR f 0 * 0.1))
