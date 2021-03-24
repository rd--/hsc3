-- rsvfhp ; changing F
let f = xLine KR 20000 20 20 RemoveSynth
in X.rsvfhp (whiteNoise 'α' AR * 0.1) f 1

-- rsvfhp ; changing Q
let q = line KR 2 0.0001 20 RemoveSynth
in X.rsvfhp (whiteNoise 'α' AR * 0.1) 12000 q

-- rsvfhp ; controls
X.rsvfhp (saw AR 200 * control KR "amp" 0.1) (control KR "freq" 440) (control KR "q" 1)

-- rsvfhp
let f = sinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in X.rsvfhp (saw AR 200 * 0.1) f (control KR "q" 1)

---- ; drawings
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.05 (X.rsvfhp (whiteNoise 'α' AR) 12000 1)
