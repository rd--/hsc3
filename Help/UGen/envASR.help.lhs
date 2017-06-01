    Sound.SC3.UGen.Help.viewSC3Help "Env.*asr"
    :i Sound.SC3.ASR

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let g = control KR "env-gate" 1
>         p = envASR 0.01 1 1 (EnvNum (-4))
>         e = envGen KR g 0.1 0 1 RemoveSynth p
>     in sinOsc AR 440 0 * e

    import Sound.OSC {- hosc -}
    withSC3 (sendMessage (n_set1 (-1) "env-gate" 0))

> e_01 =
>     [envASR 0.1 1 1 (EnvNum (-4))
>     ,envASR 0.3 0.25 1 EnvSin
>     ,envASR 0.01 0.5 1.25 EnvLin]

    import Sound.SC3.Plot {- hsc3-plot -}
    plotEnvelope e_01

An envelope with a long release time that mostly sustains and then decays quickly

> e_02 = envASR_c 0.01 1 0.75 (EnvNum (-4),EnvNum 4)
> e_03 = envASR_c 0.15 1 1.25 (EnvNum (-4),EnvNum (-4))
> e_04 = envASR_c 0.15 1 1.25 (EnvNum (-4),EnvNum 64)

    plotEnvelope [e_02,e_03,e_04]

