-- grayNoise
grayNoise 'α' ar * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.025 (grayNoise 'γ' ar)
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.1 (grayNoise 'γ' ar)
