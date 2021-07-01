-- rsvfhp ; changing F
let f = xLine kr 20000 20 20 RemoveSynth
in X.rsvfhp (whiteNoiseId 'α' ar * 0.1) f 1

-- rsvfhp ; changing Q
let q = line kr 2 0.0001 20 RemoveSynth
in X.rsvfhp (whiteNoiseId 'α' ar * 0.1) 12000 q

-- rsvfhp ; controls
X.rsvfhp (saw ar 200 * control kr "amp" 0.1) (control kr "freq" 440) (control kr "q" 1)

-- rsvfhp
let f = sinOsc kr (xLine kr 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in X.rsvfhp (saw ar 200 * 0.1) f (control kr "q" 1)

---- ; drawings
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.05 (X.rsvfhp (whiteNoiseId 'α' ar) 12000 1)
