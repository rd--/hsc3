-- dust2
dust2Id 'α' ar 200 * 0.5

-- dust2
let d = xLine kr 20000 2 10 RemoveSynth
in dust2Id 'β' ar d * 0.15

-- dust2 ; velvet noise (approx.)
let samplesPerPeriod = 20
in signum (dust2Id 'α' ar (sampleRate / samplesPerPeriod)) * 0.1

---- ; drawings
Sound.Sc3.Plot.plot_ugen1 0.1 (dust2Id 'γ' ar 400)
Sound.Sc3.Plot.plot_ugen1 0.1 (dust2Id 'γ' ar (xLine kr 5000 100 0.1 RemoveSynth))
