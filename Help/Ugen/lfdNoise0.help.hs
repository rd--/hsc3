-- lfdNoise0 ; for fast x LFNoise frequently seems stuck, LFDNoise changes smoothly
lfdNoise0Id 'α' ar (mouseX kr 0.1 1000 Exponential 0.2) * 0.1

-- lfdNoise0 ; silent for 2 secs before going up in freq
lfdNoise0Id 'α' ar (xLine kr 0.5 10000 3 RemoveSynth)

-- lfdNoise0 ; LFNoise quantizes time steps at high freqs, LFDNoise does not
lfdNoise0Id 'α' ar (xLine kr 1000 20000 10 RemoveSynth)

---- ; drawings
Sound.Sc3.Plot.plot_ugen1 0.1 (lfdNoise0Id 'γ' ar 1000)
Sound.Sc3.Plot.plot_ugen1 0.1 (lfdNoise1Id 'γ' ar 1000)
Sound.Sc3.Plot.plot_ugen1 0.1 (lfdNoise3Id 'γ' ar 1000)
