-- lfNoise2
lfNoise2 'α' AR 1000 * 0.05

-- lfNoise2 ; modulate frequency
let f = xLine KR 1000 10000 10 RemoveSynth
in lfNoise2 'α' AR f * 0.05

-- lfNoise2 ; as frequency control
let f = lfNoise2 'α' KR 4
in sinOsc AR (f * 400 + 450) 0 * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (lfNoise2 'α' AR 1000)
