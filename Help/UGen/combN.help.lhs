Sound.SC3.UGen.Help.viewSC3Help "CombN"
Sound.SC3.UGen.DB.ugenSummary "CombN"

> import Sound.SC3 {- hsc3 -}

Comb filter as resonator. The resonant fundamental is equal to
reciprocal of the delay time.

> g_01 =
>     let n = whiteNoise 'α' AR
>         dt = xLine KR 0.0001 0.01 20 RemoveSynth
>     in combN (n * 0.1) 0.01 dt 0.2

> g_02 =
>     let n = whiteNoise 'α' AR
>         dt = xLine KR 0.0001 0.01 20 RemoveSynth
>     in combL (n * 0.1) 0.01 dt 0.2

> g_03 =
>     let n = whiteNoise 'α' AR
>         dt = xLine KR 0.0001 0.01 20 RemoveSynth
>     in combC (n * 0.1) 0.01 dt 0.2

With negative feedback

> g_04 =
>     let n = whiteNoise 'α' AR
>         dt = xLine KR 0.0001 0.01 20 RemoveSynth
>     in combC (n * 0.1) 0.01 dt (-0.2)

Used as an echo.

> g_05 =
>     let d = dust 'α' AR 1
>         n = whiteNoise 'β' AR
>         i = decay (d * 0.5) 0.2 * n
>     in combC i 0.2 0.2 3
