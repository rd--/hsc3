    Sound.SC3.UGen.Help.viewSC3Help "HPF"
    Sound.SC3.UGen.DB.ugenSummary "HPF"

> import Sound.SC3

> g_01 =
>     let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
>     in hpf (saw AR 200 * 0.2) f

    import Sound.SC3.Plot.FFT {- hsc3-plot -}
    plot_ugen_fft1 0.05 (hpf (whiteNoise 'α' AR) 12000)
