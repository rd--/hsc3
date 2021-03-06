-- hpz1
let n = whiteNoiseId 'α' ar in hpz1 (n * 0.25)

-- hpz1 ; detect changes in a signal (see also hpz2)
let n = lfNoise0Id 'α' ar 1000
    h = hpz1 n
in mce [h,h `greater_than` 0,abs h `greater_than` 0]

---- ; drawings
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.05 (hpz1 (whiteNoiseId 'α' ar))


