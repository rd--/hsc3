-- ringz
ringz (dustId 'α' ar 3 * 0.3) 2000 2

-- ringz
ringz (whiteNoiseId 'α' ar * 0.005) 2000 0.5

-- ringz ; modulate frequency
let n = whiteNoiseId 'α' ar
    f = xLine kr 100 3000 10 RemoveSynth
in ringz (n * 0.005) f 0.5 * 0.2

-- ringz
let f = xLine kr 100 3000 10 RemoveSynth
in ringz (impulse ar 6 0.3) f 0.5 * 0.1

-- ringz ; modulate ring time
let rt = xLine kr 4 0.04 8 RemoveSynth
in ringz (impulse ar 6 0.3) 2000 rt * 0.1

-- ringz ; modulate ring time opposite direction
let rt = xLine kr 0.04 4 8 RemoveSynth
in ringz (impulse ar 6 0.3) 2000 rt * 0.1
