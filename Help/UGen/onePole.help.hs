-- onePole
let n = whiteNoiseId 'α' ar in onePole (n * 0.1) 0.95

-- onePole
let n = whiteNoiseId 'α' ar in onePole (n * 0.1) (-0.95)

-- onePole
let n = whiteNoiseId 'α' ar
    c = line kr (-0.99) 0.99 10 RemoveSynth
in onePole (n * 0.1) c
