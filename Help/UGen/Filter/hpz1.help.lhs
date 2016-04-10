    Sound.SC3.UGen.Help.viewSC3Help "HPZ1"
    Sound.SC3.UGen.DB.ugenSummary "HPZ1"

> import Sound.SC3 {- hsc3 -}

> g_01 = let n = whiteNoise 'α' AR in hpz1 (n * 0.25)

    import Sound.SC3.Plot.FFT {- hsc3-plot -}
    plot_ugen_fft1 0.05 (hpz1 (whiteNoise 'α' AR))

