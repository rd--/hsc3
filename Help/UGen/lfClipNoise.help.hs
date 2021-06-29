-- lfClipNoise
lfClipNoise 'α' ar 1000 * 0.05

-- lfClipNoise ; modulate frequency
let f = xLine kr 1000 10000 10 RemoveSynth
in lfClipNoise 'α' ar f * 0.05

-- lfClipNoise ; use as frequency control
let n = lfClipNoise 'α' kr 4
in sinOsc ar (n * 200 + 600) 0 * 0.1

-- lfClipNoise ; c.f. lfDClipNoise
let x = mouseX kr 0.1 1000 Exponential 0.2
    n = lfClipNoise 'β' ar x
in sinOsc ar (n * 200 + 500) 0 * 0.05

-- lfClipNoise ; c.f. lfDClipNoise
let f = xLine kr 1000 20000 10 RemoveSynth
in lfClipNoise 'δ' ar f * 0.05

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (lfClipNoise 'α' ar 1000)
