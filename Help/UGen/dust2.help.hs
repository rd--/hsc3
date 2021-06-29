-- dust2
dust2 'α' ar 200 * 0.5

-- dust2
let d = xLine kr 20000 2 10 RemoveSynth
in dust2 'β' ar d * 0.15

-- dust2 ; velvet noise (approx.)
let samplesPerPeriod = 20
in signum (dust2 'α' ar (sampleRate / samplesPerPeriod)) * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (dust2 'γ' ar 400)
Sound.SC3.Plot.plot_ugen1 0.1 (dust2 'γ' ar (xLine kr 5000 100 0.1 RemoveSynth))
