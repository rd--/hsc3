-- twoPole
let n = whiteNoiseId 'α' ar in twoPole (n * 0.005) 2000 0.95

-- twoPole
let n = whiteNoiseId 'α' ar
    f = xLine kr 800 8000 8 RemoveSynth
in twoPole (n * 0.005) f 0.95
