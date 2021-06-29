-- lfdNoise0 ; for fast x LFNoise frequently seems stuck, LFDNoise changes smoothly
lfdNoise0 'α' ar (mouseX kr 0.1 1000 Exponential 0.2) * 0.1

-- lfdNoise0 ; silent for 2 secs before going up in freq
lfdNoise0 'α' ar (xLine kr 0.5 10000 3 RemoveSynth)

-- lfdNoise0 ; LFNoise quantizes time steps at high freqs, LFDNoise does not
lfdNoise0 'α' ar (xLine kr 1000 20000 10 RemoveSynth)

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (lfdNoise0 'γ' ar 1000)
Sound.SC3.Plot.plot_ugen1 0.1 (lfdNoise1 'γ' ar 1000)
Sound.SC3.Plot.plot_ugen1 0.1 (lfdNoise3 'γ' ar 1000)
