-- lots-o-sins (jmcc) #2 ; texture=xfade,4,4,inf
let n = 60 {- n sines in each channel (twice as many during cross-fade) -}
    f0 = X.randN n 'α' 40 10000
    f1 = X.randN n 'β' 40 10000
    mk_k x = mce (replicate n x)
    o1 = klang AR 1 0 (klangSpec_mce f0 (mk_k 1) (mk_k 0))
    o2 = klang AR 1 0 (klangSpec_mce f1 (mk_k 1) (mk_k 0))
in mce2 o1 o2 * (0.1 / constant n)
