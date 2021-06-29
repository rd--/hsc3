-- allpassC ; allpass delay has no audible effect as a resonator on steady state sound
let dly = xLine kr 0.0001 0.01 20 RemoveSynth
    n = whiteNoise 'α' ar * 0.05
in allpassC n 0.01 dly 0.2

-- allpassC ; cubic variant
let dly = xLine kr 0.0001 0.01 20 RemoveSynth
    n = whiteNoise 'δ' ar * 0.05
in n + allpassC n 0.01 dly 0.2

