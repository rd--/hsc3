-- https://www.listarc.bham.ac.uk/lists/sc-users/msg21341.html (mn)
let rng :: (UGen,UGen) -> UGen -> UGen
    rng (i,j) = range i j
    fc = 400 + rng (100,200) (lfNoise2 'α' AR (mce2 1 2))
    fm = rng (100,200) (lfNoise0 'β' AR 5)
    i = rng (1,20) (lfNoise1 'γ' AR 10)
    x = sinOsc AR (fc + (sinOsc AR fm 0 * i * fm)) 0 * 0.5
    rf = rng (1000,2000) (sinOsc AR (rng (0.1,1) (lfNoise1 'δ' AR 1)) 0)
    rq = rng (0.5,10) (lfNoise1 'ε' AR 1)
    a = rlpf x rf rq
    ph = a * rng (1.0,4.0) (lfNoise1 'ζ' AR 0.1) * 4 * pi
in sinOsc AR 0.2 ph * 0.1
