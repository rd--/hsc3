-- allpassL ; linear variant
let n = whiteNoise 'γ' AR * 0.05
    dly = xLine KR 0.0001 0.01 20 RemoveSynth
in n + allpassL n 0.01 dly 0.2
