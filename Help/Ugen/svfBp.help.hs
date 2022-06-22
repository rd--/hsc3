-- svfBp ; changing F
let f = xLine kr 20 20000 20 DoNothing
in X.svfBp (whiteNoiseId 'α' ar * 0.1) f 1

-- svfBp ; changing Q
let q = line kr 10 0.0001 20 RemoveSynth
in X.svfBp (whiteNoiseId 'α' ar * 0.1) 400 q

-- svfBp ; controls
X.svfBp (saw ar 200 * control kr "amp" 0.1) (control kr "freq" 440) (control kr "q" 1)

-- svfBp
let f = sinOsc kr (xLine kr 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in X.svfBp (saw ar 200 * 0.1) f (control kr "q" 1)

---- ; drawings
Sound.Sc3.Plot.FFT.plot_ugen_fft1 0.05 (X.svfBp (whiteNoiseId 'α' ar) 900 1)
