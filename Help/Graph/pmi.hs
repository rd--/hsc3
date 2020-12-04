-- pmi ; texture=overlap,1,2,7,inf
let n = rand 'α' 1 12
    cf = rand 'β' 0 2000
    mf = rand 'γ' 0 800
    pme = rand 'δ' 0 12
    l = rand 'ε' (-1) 1
    pm = line KR 0 pme n DoNothing
in linPan2 (pmOsc AR cf mf pm 0) l 0.05
