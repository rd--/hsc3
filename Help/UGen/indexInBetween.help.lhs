> import Sound.SC3 {- hsc3 -}

Index into buffer for frequency values

> g_01 =
>     let f0 = mouseX KR 200 900 Linear 0.1
>         b = asLocalBuf 'α' [200,210,400,430,600,800]
>         i = indexInBetween b f0
>         l0 = index b i
>         l1 = index b (i + 1)
>         f1 = linLin (frac i) 0 1 l0 l1
>     in sinOsc AR (mce [f0,f1]) 0 * 0.1

> g_02 =
>     let from = asLocalBuf 'α' [1, 2, 4, 8, 16]
>         to = asLocalBuf 'β' [0, 1, 0, -1, 0]
>         x = mouseX KR 1 16 Linear 0.1
>         i = indexL to (indexInBetween from x)
>     in sinOsc AR (linLin i (-1) 1 440 880) 0 * 0.1
