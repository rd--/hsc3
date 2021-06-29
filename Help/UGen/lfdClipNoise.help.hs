-- lfdClipNoise ; for fast x lfClipNoise frequently seems stuck, lfdClipNoise changes smoothly
let x = mouseX kr 0.1 1000 Exponential 0.2
    n = lfdClipNoise 'α' ar x
in sinOsc ar (n * 200 + 500) 0 * 0.05

-- lfdClipNoise ; does not quantize time steps at high frequencies
let f = xLine kr 1000 20000 10 RemoveSynth
in lfdClipNoise 'γ' ar f * 0.05
