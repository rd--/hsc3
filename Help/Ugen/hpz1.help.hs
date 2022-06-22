-- hpz1 ; violet noise (one point difference of white noise)
hpz1 (whiteNoiseId 'α' ar) * 0.1

-- hpz1 ; blue noise (one point difference of pink noise)
hpz1 (pinkNoiseId 'α' ar) * 0.2

-- hpz1 ; detect changes in a signal (see also hpz2)
let n = lfNoise0Id 'α' ar 1000
    h = hpz1 n
in mce [h,h `greater_than` 0,abs h `greater_than` 0]

---- ; drawings
Sound.Sc3.Plot.FFT.plot_ugen_fft1 0.05 (hpz1 (whiteNoiseId 'α' ar))


