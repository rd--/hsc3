-- rRandN ; two channel sin tones
sinOsc ar (X.rRandN 2 440 442) 0 * 0.1

-- rRandN ; two channel sin tones ; id
sinOsc ar (X.rRandNId 2 'α' 440 442) 0 * 0.1

-- rRandN ; n node klang synthesis ; id
let n = 240
    f = X.rRandNId n 'α' 40 18000
    a = X.rRandNId n 'β' 0.1 0.3
    p = X.rRandNId n 'γ' (-1) 1
    s = klangSpec_mce f a p
in klang ar 1 0 s * 0.05

-- rRandN ; mce...
let f = X.rRandNId 2 'α' (mce2 440 441) 442 in sinOsc ar f 0 * 0.1
