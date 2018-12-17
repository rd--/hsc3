    Sound.SC3.UGen.Help.viewSC3Help "Hasher"
    Sound.SC3.UGen.DB.ugenSummary "Hasher"

> import Sound.SC3 {- hsc3 -}

noise

> g_01 = hasher (line AR 0 1 1 RemoveSynth) * 0.2

remap x

> f_02 x_f =
>   let x = mouseX KR 0 10 Linear 0.2
>       f = hasher (x_f x) * 300 + 500
>   in sinOsc AR f 0 * 0.1

> g_02 = f_02 (\x -> roundTo x 1)

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen_nrt (400,1) 1.0 (hasher (line AR 0 1 1 RemoveSynth))

> g_03 = f_02 id

> g_04 = sinOsc AR (whiteNoise 'α' KR * 300 + 500) 0 * 0.1
