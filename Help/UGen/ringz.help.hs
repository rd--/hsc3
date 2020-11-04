-- ringz
ringz (dust 'α' AR 3 * 0.3) 2000 2

-- ringz
ringz (whiteNoise 'α' AR * 0.005) 2000 0.5

-- ringz ; modulate frequency
let n = whiteNoise 'α' AR
    f = xLine KR 100 3000 10 RemoveSynth
in ringz (n * 0.005) f 0.5 * 0.2

-- ringz
let f = xLine KR 100 3000 10 RemoveSynth
in ringz (impulse AR 6 0.3) f 0.5 * 0.1

-- ringz ; modulate ring time
let rt = xLine KR 4 0.04 8 RemoveSynth
in ringz (impulse AR 6 0.3) 2000 rt * 0.1

-- ringz ; modulate ring time opposite direction
let rt = xLine KR 0.04 4 8 RemoveSynth
in ringz (impulse AR 6 0.3) 2000 rt * 0.1
