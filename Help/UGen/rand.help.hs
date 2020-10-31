-- rand
let f = rand 'α' 200 1200
    l = rand 'β' (-1) 1
    e = line KR 0.2 0 0.1 RemoveSynth
    o = fSinOsc AR f 0
in pan2 (o * e) l 1
