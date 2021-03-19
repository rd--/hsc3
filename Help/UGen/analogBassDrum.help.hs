-- analogBassDrum
let trig = dust2 'α' KR 8
    infsustain = 0.0
    accent = 0.25
    freq = tExpRand 'β' 40 120 trig
    tone = tRand 'γ' 0.0 0.35 trig
    decay = 0.15
    attackfm = tRand 'δ' 0.1 0.2 trig
    selffm = tRand 'ε' 0.1 0.9 trig
    sig = X.analogBassDrum AR trig infsustain accent freq tone decay attackfm selffm
in pan2 sig (tRand 'ζ' (-1) 1 trig) 1
