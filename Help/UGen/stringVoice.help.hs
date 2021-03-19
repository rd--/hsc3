-- stringVoice
let pan = 0
    freq = 100
    trig = dust2 'α' KR 7
    infsustain = 0
    accent = tRand 'β' 0 1 trig
    structure = tRand 'γ' 0 1 trig
    brightness = tRand 'δ' 0 0.5 trig
    damping = tRand 'ε' 0.1 0.5 trig
    sig = X.stringVoice AR trig infsustain freq accent structure brightness damping
in pan2 sig pan 1
