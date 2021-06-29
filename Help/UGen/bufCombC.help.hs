-- bufCombC ; comb filter as resonator ; resonant fundamental is reciprocal of the delay time
let b = localBuf 'α' 1 44100
    n = whiteNoise 'β' ar
    dt = xLine kr 0.0001 0.01 20 RemoveSynth
in bufCombC b (n * 0.1) dt 0.2 * 0.05

-- bufCombC ; with negative feedback
let b = localBuf 'α' 1 44100
    n = whiteNoise 'β' ar
    dt = xLine kr 0.0001 0.01 20 RemoveSynth
in bufCombC b (n * 0.1) dt (-0.2) * 0.05

-- bufCombC ; used as an echo (filtered decaying noise bursts)
let b = localBuf 'α' 1 44100
    d = dust 'β' ar 1
    n = whiteNoise 'γ' ar
    i = decay (d * 0.5) 0.2 * n
in bufCombC b i 0.2 3

