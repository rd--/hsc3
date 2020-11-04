-- hpf
let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in hpf (saw AR 200 * 0.1) f

---- ; drawings
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.05 (hpf (whiteNoise 'α' AR) 12000)
