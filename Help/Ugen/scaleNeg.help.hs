-- scaleNeg
scaleNeg (fSinOsc ar 500 0) (line ar 1 (-1) 4 RemoveSynth) * 0.1

-- scaleNeg ; written out
let o = fSinOsc ar 500 0
    l = line ar 1 (-1) 4 RemoveSynth
    c = o `less_than` 0
in (c * (o * l) + (1 - c) * o) * 0.1
