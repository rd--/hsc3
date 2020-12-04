-- pond life (jmcc) #1 ; texture=overlap,8,8,4,inf
let f0 = 20 + rand 'α' 0 30
    f1 = fSinOsc KR f0 0 * rand 'β' 100 400 + linRand 'γ' 500 2500 0
    a = lfPulse KR (3 / rand 'δ' 1 9) 0 (rand 'ε' 0.2 0.5) * 0.04
in pan2 (sinOsc AR f1 0 * a) (rand 'ζ' (-1) 1) 0.5
