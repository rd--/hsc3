-- grayNoise
grayNoiseId 'α' ar * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.025 (grayNoiseId 'γ' ar)
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.1 (grayNoiseId 'γ' ar)
