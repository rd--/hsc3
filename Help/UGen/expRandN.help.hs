-- expRandN ; two channel sin tones
sinOsc ar (X.expRandNId 2 'α' 440 880) 0 * 0.1

-- expRandN ; n node klang synthesis
let n = 240
    f = X.expRandNId n 'α' 40 18000
    a = X.expRandNId n 'β' 0.1 0.3
    p = X.randNId n 'γ' (-1) 1
    s = klangSpec_mce f a p
in klang ar 1 0 s * 0.01

-- expRandN ; mce...
let f = X.expRandNId 2 'α' (mce2 440 441) 442 in sinOsc ar f 0 * 0.1
