-- rand
let f = rand 'α' 200 1200
    l = rand 'β' (-1) 1
    e = line KR 0.2 0 0.1 RemoveSynth
    o = fSinOsc AR f 0
in pan2 (o * e) l 1

-- rand
sinOsc AR (lfNoise1 'α' KR (6 + mce2 (rand 'β' (-4) 4) (rand 'γ' (-4) 4)) * 100 + 200) 0 * 0.1
