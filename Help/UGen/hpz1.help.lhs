> import Sound.SC3 {- hsc3 -}

> g_01 = let n = whiteNoise 'α' AR in hpz1 (n * 0.25)

    > import Sound.SC3.Plot.FFT {- hsc3-plot -}
    > plot_ugen_fft1 0.05 (hpz1 (whiteNoise 'α' AR))

detect changes in a signal (see also hpz2)

> g_02 =
>   let n = lfNoise0 'α' AR 1000
>       h = hpz1 n
>   in mce [h,h >** 0,abs h >** 0]

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen 0.01 g_02
