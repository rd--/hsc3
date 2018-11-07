    Sound.SC3.UGen.Help.viewSC3Help "Hasher"
    Sound.SC3.UGen.DB.ugenSummary "Hasher"

> import Sound.SC3 {- hsc3 -}

noise

> g_01 = hasher (line AR 0 1 1 RemoveSynth) * 0.2

remap x

> g_02 =
>   let x = mouseX KR 0 10 Linear 0.2
>       f = hasher (roundTo x 1) * 300 + 500
>   in sinOsc AR f 0 * 0.1

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen_nrt (400,1) 1.0 (hasher (line AR 0 1 1 RemoveSynth))
