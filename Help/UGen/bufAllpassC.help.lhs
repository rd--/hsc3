    > Sound.SC3.UGen.Help.viewSC3Help "BufAllpassC"
    > Sound.SC3.UGen.DB.ugenSummary "BufAllpassC"

> import Sound.SC3 {- hsc3 -}

Allocate buffer

    > withSC3 (async (b_alloc 0 44100 1))

Filtered decaying noise bursts

> g_01 =
>     let d = dust 'α' AR 1
>         n = whiteNoise 'β' AR
>         x = decay d 0.2 * n * 0.25
>     in bufAllpassC 0 x 0.25 6
