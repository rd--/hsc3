-- tiRand
let l = tiRandId 'α' (-1) 1 (dustId 'β' kr 10)
in pan2 (pinkNoiseId 'γ' ar * 0.1) l 1

-- tiRand ; monadic variant
uid_st_eval
 (do l <- tiRandM (-1) 1 =<< dustM kr 10
     n <- pinkNoiseM ar
     return (pan2 (n * 0.1) l 1))

-- tiRand ; osc frequency
uid_st_eval
 (do n <- tiRandM 4 12 =<< dustM kr 10
     let f = n * 150 + (mce [0,1])
     return (sinOsc ar f 0 * 0.1))
