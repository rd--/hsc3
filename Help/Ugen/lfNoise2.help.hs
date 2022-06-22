-- lfNoise2
lfNoise2Id 'α' ar 1000 * 0.05

-- lfNoise2 ; modulate frequency
let f = xLine kr 1000 10000 10 RemoveSynth
in lfNoise2Id 'α' ar f * 0.05

-- lfNoise2 ; as frequency control
let f = lfNoise2Id 'α' kr 4
in sinOsc ar (f * 400 + 450) 0 * 0.1

---- ; drawings ; lfNoise2 is bi-polar
Sound.SC3.Plot.plot_ugen1 0.1 (lfNoise2Id 'α' ar 1000)
