-- scaleNeg
scaleNeg (fSinOsc AR 500 0) (line AR 1 (-1) 4 RemoveSynth) * 0.1

-- scaleNeg ; written out
let o = fSinOsc AR 500 0
    l = line AR 1 (-1) 4 RemoveSynth
    c = o `less_than` 0
in (c * (o * l) + (1 - c) * o) * 0.1
