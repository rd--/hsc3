-- twoPole
let n = whiteNoise 'α' AR in twoPole (n * 0.005) 2000 0.95

-- twoPole
let n = whiteNoise 'α' AR
    f = xLine KR 800 8000 8 RemoveSynth
in twoPole (n * 0.005) f 0.95
