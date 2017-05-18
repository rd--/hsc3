    Sound.SC3.UGen.Help.viewSC3Help "Env.*sine"
    :t Sound.SC3.envSine

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let s = envSine 9 0.1
>         e = envGen KR 1 1 0 1 RemoveSynth s
>     in sinOsc AR 440 0 * e

> e_01 = [envSine 9 1,envSine 3 0.25]

    import Sound.SC3.Plot {- hsc3-plot -}
    plotEnvelope e_01
