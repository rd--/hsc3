-- hell is busy (jmcc) #1 ; texture=overlap,4,4,8,inf
let o = fSinOsc AR (400 + rand 'α' 0 2000) 0
    a = lfPulse KR (1 + rand 'β' 0 10.0) 0 (rand 'γ' 0 0.7) * 0.04
in pan2 (o * a) (rand 'δ' (-1) 1) 1
