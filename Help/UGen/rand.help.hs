-- rand
let f = rand 'α' 200 1200
    l = rand 'β' (-1) 1
    e = line kr 0.2 0 0.1 RemoveSynth
    o = fSinOsc ar f 0
in pan2 (o * e) l 1

-- rand
sinOsc ar (lfNoise1 'α' kr (6 + mce2 (rand 'β' (-4) 4) (rand 'γ' (-4) 4)) * 100 + 200) 0 * 0.1
