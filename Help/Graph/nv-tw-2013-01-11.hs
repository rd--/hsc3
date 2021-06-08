-- https://twitter.com/headcube/status/289761321065541633 (nv)
let nc = 80
    i = inFeedback nc 20
    n = mceFillInt nc (\z -> lfNoise1 z KR 0.001 + 1) / constant nc
    j = combL (lpf i 2000) 1 n 0.05
    d = mceFillInt nc (\z -> dust2 z AR 0.01)
    x = sin (j + d)
in mceConcat [splay x 1 1 0 True
             ,mce (replicate 18 0)
             ,(x - mceRotate 1 x) / 2]
