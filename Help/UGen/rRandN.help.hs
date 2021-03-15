-- rRandN ; two channel sin tones
sinOsc AR (X.rRandN 2 'α' 440 442) 0 * 0.1

-- rRandN ; n node klang synthesis
let n = 240
    f = X.rRandN n 'α' 40 18000
    a = X.rRandN n 'β' 0.1 0.3
    p = X.rRandN n 'γ' (-1) 1
    s = klangSpec_mce f a p
in klang AR 1 0 s * 0.05

-- rRandN ; mce...
let f = X.rRandN 2 'α' (mce2 440 441) 442 in sinOsc AR f 0 * 0.1
