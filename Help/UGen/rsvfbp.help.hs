-- rsvfbp ; changing F
let f = xLine kr 20 20000 20 DoNothing
in X.rsvfbp (whiteNoiseId 'α' ar * 0.1) f 1

-- rsvfbp ; changing Q
let q = line kr 10 0.0001 20 RemoveSynth
in X.rsvfbp (whiteNoiseId 'α' ar * 0.1) 400 q

-- rsvfbp ; controls
X.rsvfbp (saw ar 200 * control kr "amp" 0.1) (control kr "freq" 440) (control kr "q" 1)

-- rsvfbp
let f = sinOsc kr (xLine kr 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in X.rsvfbp (saw ar 200 * 0.1) f (control kr "q" 1)

---- ; drawings
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.05 (X.rsvfbp (whiteNoiseId 'α' ar) 900 1)
