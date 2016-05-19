    > Sound.SC3.UGen.Help.viewSC3Help "BufCombC"
    > Sound.SC3.UGen.DB.ugenSummary "BufCombC"

> import Sound.SC3 {- hsc3 -}

Allocate buffer zero (required for examples below)

    > withSC3 (async (b_alloc 0 44100 1))

Comb filter as resonator. The resonant fundamental is equal to
reciprocal of the delay time.

> g_01 =
>     let n = whiteNoise 'α' AR
>         dt = xLine KR 0.0001 0.01 20 RemoveSynth
>     in bufCombC 0 (n * 0.1) dt 0.2

With negative feedback

> g_02 =
>     let n = whiteNoise 'α' AR
>         dt = xLine KR 0.0001 0.01 20 RemoveSynth
>     in bufCombC 0 (n * 0.1) dt (-0.2)

Used as an echo (filtered decaying noise bursts)

> g_03 =
>     let d = dust 'α' AR 1
>         n = whiteNoise 'β' AR
>         i = decay (d * 0.5) 0.2 * n
>     in bufCombC 0 i 0.2 3

