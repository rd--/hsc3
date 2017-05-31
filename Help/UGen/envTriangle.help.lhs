    Sound.SC3.UGen.Help.viewSC3Help "Env.*triangle"
    :t envTriangle

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let t = envTriangle 1 0.1
>         e = envGen KR 1 1 0 1 RemoveSynth t
>     in sinOsc AR 440 0 * e

> e_02 = [envTriangle 1 1,envTriangle 0.25 0.5]

    import Sound.SC3.Plot
    plotEnvelope e_02
