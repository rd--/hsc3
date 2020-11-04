-- onePole
let n = whiteNoise 'α' AR in onePole (n * 0.1) 0.95

-- onePole
let n = whiteNoise 'α' AR in onePole (n * 0.1) (-0.95)

-- onePole
let n = whiteNoise 'α' AR
    c = line KR (-0.99) 0.99 10 RemoveSynth
in onePole (n * 0.1) c
