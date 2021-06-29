-- lfNoise1
lfNoise1 'α' ar 1000 * 0.05

-- lfNoise1 ; modulate frequency
let f = xLine kr 1000 10000 10 RemoveSynth
in lfNoise1 'α' ar f * 0.05

-- lfNoise1 ; as frequency control
let n = lfNoise1 'α' kr 4
    f = n * 400 + 450
in sinOsc ar f 0 * 0.1
