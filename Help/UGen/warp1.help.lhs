    Sound.SC3.UGen.Help.viewSC3Help "Warp1"
    Sound.SC3.UGen.DB.ugenSummary "Warp1"

> import Sound.SC3 {- hsc3 -}

    > let fn = "/home/rohan/data/audio/pf-c5.aif"
    > withSC3 (async (b_allocRead 10 fn 0 0))

> g_01 =
>     let p = linLin (lfSaw KR 0.05 0) (-1) 1 0 1
>         x = mouseX KR 0.5 2 Linear 0.1
>     in warp1 1 10 p x 0.1 (-1) 8 0.1 2

real-time (delayed) input

    > withSC3 (async (b_alloc 10 8192 1))

> g_02 =
>     let i = soundIn 4
>         r = recordBuf AR 10 0 1 0 1 Loop 1 DoNothing i
>         ph = (8192 / sampleRate) * 2 * pi
>         p = lfSaw KR (1 / bufDur KR 10) ph * 0.5 + 0.5
>         x = mouseX KR 0.5 2 Linear 0.2
>         y = mouseY KR 0.01 0.2 Linear 0.2
>         w = warp1 1 10 p x 0.1 (-1) 8 y 4
>     in mrg2 (i + w) r
