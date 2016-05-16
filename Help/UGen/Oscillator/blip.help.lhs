    Sound.SC3.UGen.Help.viewSC3Help "Blip"
    Sound.SC3.UGen.DB.ugenSummary "Blip"

> import Sound.SC3 {- hsc3 -}

> g_01 = blip AR 440 200 * 0.1

Modulate frequency

> g_02 = let f = xLine KR 20000 200 6 RemoveSynth in blip AR f 100 * 0.1

Modulate number of harmonics.

> g_03 = let nh = line KR 1 100 20 RemoveSynth in blip AR 200 nh * 0.2

Self-modulation at control rate.

> g_04 =
>     let fr = blip KR 0.25 3 * 300 + 500
>         nh = blip KR 0.15 2 * 20 + 21
>     in blip AR fr nh * 0.2

Drawings

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen1 0.1 (blip AR 1000 20)

    import Sound.SC3.Plot.FFT {- hsc3-plot -}
    plot_ugen_fft1 0.1 (blip AR 1000 20)

![](sw/hsc3/Help/SVG/blip.0.svg)
![](sw/hsc3/Help/SVG/blip.1.svg)
