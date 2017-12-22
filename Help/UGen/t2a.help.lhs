    Sound.SC3.UGen.Help.viewSC3Help "T2A"
    Sound.SC3.UGen.DB.ugenSummary "T2A"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let tr = impulse KR (mouseX KR 1 100 Exponential 0.2) 0
>     in ringz (t2a tr 0) 800 0.01 * 0.4

compare with k2a (oscilloscope)

> g_02 =
>     let tr = impulse KR 200 0
>     in lag (mce2 (t2a tr 0) (k2a tr)) 0.001

removing jitter by randomising offset

> g_03 =
>     let tr = impulse KR (mouseX KR 1 100 Exponential 0.2) 0
>         o = range 0 (blockSize - 1) (whiteNoise 'β' KR)
>     in ringz (t2a tr o) 880 0.1 * 0.4

