-- analogSnareDrum
let trig = dust2 'α' KR 8
    infsustain = 0.0
    accent = 0.25
    freq = tExpRand 'β' 40 220  trig
    tone = tRand 'γ' 0.0 0.4 trig
    decay = tRand 'δ' 0.1 0.8 trig
    snappy = tRand 'ε' 0.0 0.9  trig
    sig = X.analogSnareDrum AR trig infsustain accent freq tone decay snappy
in pan2 sig (tRand 'ζ' (-1) 1 trig) 0.1
