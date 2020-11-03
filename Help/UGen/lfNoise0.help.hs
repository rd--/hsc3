-- lfNoise0
lfNoise0 'α' AR 1000 * 0.05

-- lfNoise0 ; modulate frequency
let f = xLine KR 1000 10000 10 RemoveSynth
in lfNoise0 'α' AR f * 0.05

-- lfNoise0 ; as frequency control
let f = lfNoise0 'α' KR 4
in sinOsc AR (f * 400 + 450) 0 * 0.1

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
plot_ugen1 0.1 (lfNoise0 'γ' AR 1000)
plot_ugen1 0.1 (lfNoise1 'γ' AR 1000)
plot_ugen1 0.1 (lfNoise2 'γ' AR 1000)
