-- rExpRandN ; two channel sin tones
sinOsc ar (X.rExpRandNId 2 'α' 440 880) 0 * 0.1

-- rExpRandN ; n node klang synthesis
let n = 240
    f = X.rExpRandNId n 'α' 40 18000
    a = X.rExpRandNId n 'β' 0.1 0.3
    p = X.rRandNId n 'γ' (-1) 1
    s = klangSpec_mce f a p
in klang ar 1 0 s * 0.01

-- rExpRandN ; mce...
let f = X.rExpRandNId 2 'α' (mce2 440 441) 442 in sinOsc ar f 0 * 0.1
