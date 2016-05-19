    > Sound.SC3.UGen.Help.viewSC3Help "IndexInBetween"
    > Sound.SC3.UGen.DB.ugenSummary "IndexInBetween"

> import Sound.SC3 {- hsc3 -}

Allocate and set values at buffer ten

    > withSC3 (async (b_alloc_setn1 10 0 [200,210,400,430,600,800]))

Index into buffer for frequency values

> g_01 =
>     let f0 = mouseX KR 200 900 Linear 0.1
>         i = indexInBetween 10 f0
>         l0 = index 10 i
>         l1 = index 10 (i + 1)
>         f1 = linLin (frac i) 0 1 l0 l1
>     in sinOsc AR (mce [f0,f1]) 0 * 0.1

Free buffer

    > withSC3 (send (b_free 10))

> g_02 =
>     let from = asLocalBuf 'α' [1, 2, 4, 8, 16]
>         to = asLocalBuf 'β' [0, 1, 0, -1, 0]
>         x = mouseX KR 1 16 Linear 0.1
>         i = indexL KR to (indexInBetween from x)
>     in sinOsc AR (linLin i (-1) 1 440 880) 0 * 0.1
