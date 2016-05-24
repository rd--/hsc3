    > Sound.SC3.UGen.Help.viewSC3Help "FMGrain"
    > Sound.SC3.UGen.DB.ugenSummary "FMGrain"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let t = impulse AR 20 0
>         n = linLin (lfNoise1 'α' KR 1) (-1) 1 1 10
>         s = envSine 9 0.1
>         e = envGen KR 1 1 0 1 RemoveSynth s
>     in fmGrain t 0.2 440 220 n * e
