-- dust2
dust2 'α' AR 200 * 0.5

-- dust2
let d = xLine KR 20000 2 10 RemoveSynth
in dust2 'β' AR d * 0.15

-- dust2 ; velvet noise (approx.)
let samplesPerPeriod = 20
in signum (dust2 'α' AR (sampleRate / samplesPerPeriod)) * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (dust2 'γ' AR 400)
Sound.SC3.Plot.plot_ugen1 0.1 (dust2 'γ' AR (xLine KR 5000 100 0.1 RemoveSynth))
