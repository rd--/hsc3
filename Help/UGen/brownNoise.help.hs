-- brownNoise
brownNoise 'α' AR * 0.1

-- brownNoise ; KR rate noise as frequency control
let n = brownNoise 'α' KR
in sinOsc AR (linExp n (-1) 1 64 9600) 0 * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (brownNoise 'γ' AR)
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.1 (brownNoise 'γ' AR)
UI.ui_sc3_scope_freq (600,400) 0
