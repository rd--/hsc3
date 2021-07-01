-- oneZero
let n = whiteNoiseId 'α' ar in oneZero (n * 0.1) 0.5

-- oneZero
let n = whiteNoiseId 'α' ar in oneZero (n * 0.1) (-0.5)

-- oneZero
let n = whiteNoiseId 'α' ar
    c = line kr (-0.5) 0.5 10 RemoveSynth
in oneZero (n * 0.1) c
