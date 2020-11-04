-- lfdClipNoise ; for fast x lfClipNoise frequently seems stuck, lfdClipNoise changes smoothly
let x = mouseX KR 0.1 1000 Exponential 0.2
    n = lfdClipNoise 'α' AR x
in sinOsc AR (n * 200 + 500) 0 * 0.05

-- lfdClipNoise ; does not quantize time steps at high frequencies
let f = xLine KR 1000 20000 10 RemoveSynth
in lfdClipNoise 'γ' AR f * 0.05
