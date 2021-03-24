-- rsvfbp ; changing F
let f = xLine KR 20 20000 20 DoNothing
in X.rsvfbp (whiteNoise 'α' AR * 0.1) f 1

-- rsvfbp ; changing Q
let q = line KR 10 0.0001 20 RemoveSynth
in X.rsvfbp (whiteNoise 'α' AR * 0.1) 400 q

-- rsvfbp ; controls
X.rsvfbp (saw AR 200 * control KR "amp" 0.1) (control KR "freq" 440) (control KR "q" 1)

-- rsvfbp
let f = sinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in X.rsvfbp (saw AR 200 * 0.1) f (control KR "q" 1)

---- ; drawings
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.05 (X.rsvfbp (whiteNoise 'α' AR) 900 1)
