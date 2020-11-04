-- oneZero
let n = whiteNoise 'α' AR in oneZero (n * 0.1) 0.5

-- oneZero
let n = whiteNoise 'α' AR in oneZero (n * 0.1) (-0.5)

-- oneZero
let n = whiteNoise 'α' AR
    c = line KR (-0.5) 0.5 10 RemoveSynth
in oneZero (n * 0.1) c
