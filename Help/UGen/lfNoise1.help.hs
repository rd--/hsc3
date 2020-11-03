-- lfNoise1
lfNoise1 'α' AR 1000 * 0.05

-- lfNoise1 ; modulate frequency
let f = xLine KR 1000 10000 10 RemoveSynth
in lfNoise1 'α' AR f * 0.05

-- lfNoise1 ; as frequency control
let n = lfNoise1 'α' KR 4
    f = n * 400 + 450
in sinOsc AR f 0 * 0.1
