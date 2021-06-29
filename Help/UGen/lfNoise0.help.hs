-- lfNoise0
lfNoise0 'α' ar 1000 * 0.05

-- lfNoise0 ; modulate frequency
let f = xLine kr 1000 10000 10 RemoveSynth
in lfNoise0 'α' ar f * 0.05

-- lfNoise0 ; as frequency control
let f = lfNoise0 'α' kr 4
in sinOsc ar (f * 400 + 450) 0 * 0.1

-- lfNoise0 ; lfdNoise0
lfNoise0 'α' ar (mouseX kr 0.1 1000 Exponential 0.2) * 0.1

-- lfNoise0 ; lfdNoise0
lfNoise0 'α' ar (xLine kr 0.5 10000 3 RemoveSynth)

-- lfNoise0 ; lfdNoise0
lfNoise0 'α' ar (xLine kr 1000 20000 10 RemoveSynth)

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
plot_ugen1 0.1 (lfNoise0 'γ' ar 1000)
plot_ugen1 0.1 (lfNoise1 'γ' ar 1000)
plot_ugen1 0.1 (lfNoise2 'γ' ar 1000)
