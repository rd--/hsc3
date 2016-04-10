    Sound.SC3.UGen.Help.viewSC3Help "BPF"
    Sound.SC3.UGen.DB.ugenSummary "BPF"

> import Sound.SC3

> g_01 =
>     let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
>     in bpf (saw AR 200 * 0.5) f 0.3

> g_02 =
>     let n = whiteNoise 'α' AR
>         x = mouseX KR 220 440 Exponential 0.1
>         y = mouseY KR 0.01 0.2 Linear 0.1
>     in bpf n (mce [x, 550 - x]) y

    import Sound.SC3.Plot.FFT {- hsc3-plot -}
    plot_ugen_fft1 0.05 (bpf (whiteNoise 'α' AR) 440 0.01)
