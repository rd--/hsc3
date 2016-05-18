    > Sound.SC3.UGen.Help.viewSC3Help "K2A"
    > Sound.SC3.UGen.DB.ugenSummary "K2A"

> import Sound.SC3 {- hsc3 -}

> g_01 = k2A (whiteNoise 'α' KR * 0.3)

compare

> g_02 = mce2 (k2A (whiteNoise 'α' KR * 0.3)) (whiteNoise 'β' AR * 0.1)

> g_03 =
>     let blockSize = controlDur * sampleRate
>         freq = (mouseX KR 0.1 40 Exponential 0.2) / blockSize * sampleRate;
>         o1 = k2A (lfNoise0 'α' KR freq)
>         o2 = lfNoise0 'β' AR freq
>     in mce2 o1 o2 * 0.1
