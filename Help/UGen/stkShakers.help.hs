-- stkShakers ; default param ; https://ccrma.stanford.edu/software/stk/classstk_1_1Shakers.html
X.stkShakers AR 0.0 64.0 64.0 64.0 64.0

-- stkShakers ; random param
let x = mouseX KR 0.25 4 Linear 0.2
    tr = impulse KR x 0 - 0.5
    instr = tRand 'α' 0 23 tr
    energy = tRand 'β' 0 127 tr
    decay_ = tRand 'γ' 0 127 tr
    objects = tRand 'δ' 0 127 tr
    resfreq = tRand 'ε' 0 127 tr
in X.stkShakers AR instr energy decay_ objects resfreq

-- stkShakers ; noise param
let walk z = linLin (lfNoise2 z KR 0.75) (-1) 1
    instr = walk 'α' 0 23
    energy = walk 'β' 0 127
    decay_ = walk 'γ' 0 127
    objects = walk 'δ' 0 127
    resfreq = walk 'ε' 0 127
in pan2 (X.stkShakers AR instr energy decay_ objects resfreq) (lfNoise2 'ζ' KR 0.75) 1
