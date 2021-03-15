-- ring modulated klank (jmcc) #2 ; texture=overlap,4,4,4,inf
let p = 8
    k = let sp = klankSpec_mce (X.rRandN p 'α' 100 10000)
                               (mce (replicate p 1))
                               (X.rRandN p 'α' 0.2 1)
        in klank (dust 'α' AR 20 * 0.02) 1 0 1 sp
    f = lfNoise2 'α' KR (rand 'α' 0.1 0.4) * 200 + rand 'α' 350 400
in pan2 (sinOsc AR f 0 * k) (rand 'α' (-1) 1) 1
