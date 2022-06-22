-- grayNoise
grayNoiseId 'α' ar * 0.1

---- ; drawings
Sound.Sc3.Plot.plot_ugen1 0.025 (grayNoiseId 'γ' ar)
Sound.Sc3.Plot.Fft.plot_ugen_fft1 0.1 (grayNoiseId 'γ' ar)
