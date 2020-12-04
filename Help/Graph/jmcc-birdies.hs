-- birdies (jmcc) #6 ; texture=overlap,7,4,4,inf
let p1 = lfPulse KR (0.4 + rand 'α' 0 1) 0 (rand 'β' 0 0.8 + 0.1) * (rand 'γ' 0 3 + 4) + 2
    p2 = lfPulse KR (0.4 + rand 'δ' 0 1) 0 (rand 'ε' 0 0.8 + 0.1) * (rand 'ζ' 0 3 + 4)
    p3 = lfPulse KR (0.2 + rand 'η' 0 0.5) 0 0.4 * 0.02
    sw = lfSaw KR (p1 + p2) 0 * (- (1000 + rand 'θ' 0 800)) + (4000 + rand2 'ι' 1200)
    freq = lag sw 0.05
    amp = lag p3 0.3
in pan2 (sinOsc AR freq 0 * amp) (rand2 'κ' 1) 1
