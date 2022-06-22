-- hpf
let f = fSinOsc kr (xLine kr 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in hpf (saw ar 200 * 0.1) f

-- hpf ; cutoff at one hertz
hpf (whiteNoiseId 'α' ar) 1 * 0.1

---- ; drawings
Sound.Sc3.Plot.FFT.plot_ugen_fft1 0.05 (hpf (whiteNoiseId 'α' ar) 12000)
