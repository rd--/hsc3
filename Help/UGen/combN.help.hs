-- combN ; comb filter as resonator, the resonant fundamental is equal to reciprocal of the delay time
let n = whiteNoiseId 'α' ar * 0.02
    dt = xLine kr 0.0001 0.01 20 RemoveSynth
in combN n 0.01 dt 0.2
