-- https://www.listarc.bham.ac.uk/lists/sc-users/msg21341.html (mn)
let fc = 400 + (lfNoise2 'α' ar (mce2 1 2) `in_range` (100,200))
    fm = lfNoise0 'β' ar 5 `in_range` (100,200)
    i = lfNoise1 'γ' ar 10 `in_range` (1,20)
    x = sinOsc ar (fc + (sinOsc ar fm 0 * i * fm)) 0 * 0.5
    rf = sinOsc ar (lfNoise1 'δ' ar 1 `in_range` (0.1,1)) 0 `in_range` (1000,2000)
    rq = lfNoise1 'ε' ar 1 `in_range` (0.5,10)
    a = rlpf x rf rq
    ph = a * (lfNoise1 'ζ' ar 0.1 `in_range` (1.0,4.0)) * 4 * pi
in sinOsc ar 0.2 ph * 0.1
