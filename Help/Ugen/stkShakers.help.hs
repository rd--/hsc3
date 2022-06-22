-- stkShakers ; default param ; https://ccrma.stanford.edu/software/stk/classstk_1_1Shakers.html
X.stkShakers ar 0.0 64.0 64.0 64.0 64.0

-- stkShakers ; random param
let x = mouseX kr 0.25 4 Linear 0.2
    tr = impulse kr x 0 - 0.5
    instr = tRandId 'α' 0 23 tr
    energy = tRandId 'β' 0 127 tr
    decay_ = tRandId 'γ' 0 127 tr
    objects = tRandId 'δ' 0 127 tr
    resfreq = tRandId 'ε' 0 127 tr
in X.stkShakers ar instr energy decay_ objects resfreq

-- stkShakers ; noise param
let walkId z = linLin (lfNoise2Id z kr 0.75) (-1) 1
    instr = walkId 'α' 0 23
    energy = walkId 'β' 0 127
    decay_ = walkId 'γ' 0 127
    objects = walkId 'δ' 0 127
    resfreq = walkId 'ε' 0 127
in pan2 (X.stkShakers ar instr energy decay_ objects resfreq) (lfNoise2Id 'ζ' kr 0.75) 1
