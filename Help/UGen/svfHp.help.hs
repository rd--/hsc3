-- svfHp ; changing F
let f = xLine kr 20000 20 20 RemoveSynth
in X.svfHp (whiteNoiseId 'α' ar * 0.1) f 1

-- svfHp ; changing Q
let q = line kr 2 0.0001 20 RemoveSynth
in X.svfHp (whiteNoiseId 'α' ar * 0.1) 12000 q

-- svfHp ; controls
X.svfHp (saw ar 200 * control kr "amp" 0.1) (control kr "freq" 440) (control kr "q" 1)

-- svfHp
let f = sinOsc kr (xLine kr 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in X.svfHp (saw ar 200 * 0.1) f (control kr "q" 1)

---- ; drawings
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.05 (X.svfHp (whiteNoiseId 'α' ar) 12000 1)
