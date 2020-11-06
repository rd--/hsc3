-- bufCombL ; comb filter as resonator ; resonant fundamental is reciprocal of the delay time
let b = localBuf 'α' 1 44100
    n = whiteNoise 'β' AR
    dt = xLine KR 0.0001 0.01 20 RemoveSynth
in bufCombL b (n * 0.1) dt 0.2 * 0.05

