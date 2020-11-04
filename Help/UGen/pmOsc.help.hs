-- pmOsc ; composite of sinOsc, ie. sinOsc r cf (sinOsc r mf mp * pm) ; modulate carfreq
pmOsc AR (line KR 600 900 5 DoNothing) 600 3 0 * 0.1

-- pmOsc ; modulate modfreq
pmOsc AR 300 (line KR 600 900 5 DoNothing) 3 0 * 0.1

-- pmOsc ; modulate index
pmOsc AR 300 550 (line KR 0 20 8 DoNothing) 0 * 0.1

-- pmOsc ; random parameters, linear modulation index motion over n seconds
let n = 2
    cf = rand 'α' 0 2000
    mf = rand 'β' 0 800
    pme = rand 'γ' 0 12
    l = rand 'δ' (-1) 1
    pm = line KR 0 pme n DoNothing
in linPan2 (pmOsc AR cf mf pm 0) l 0.05
