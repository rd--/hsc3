    > Sound.SC3.UGen.Help.viewSC3Help "T2K"
    > Sound.SC3.UGen.DB.ugenSummary "T2K"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let tr = impulse KR (mouseX KR 1 100 Exponential 0.2) 0
>     in ringz (t2A tr 0) 800 0.01 * 0.4

compare with K2A (oscilloscope)

> g_02 =
>     let tr = impulse KR 200 0
>     in lag (mce2 (t2A tr 0) (k2A tr)) 0.001

removing jitter by randomising offset (C-cC-a at g_03)

> g_03 =
>     let tr = impulse KR (mouseX KR 1 100 Exponential 0.2) 0
>         o = range 0 (blockSize - 1) (whiteNoise 'α' KR)
>     in ringz (t2A tr o) 880 0.1 * 0.4

