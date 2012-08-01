> Sound.SC3.UGen.Help.viewSC3Help "VDiskIn"
> Sound.SC3.UGen.DB.ugenSummary "VDiskIn"

> import Sound.SC3

> let {f = "/home/rohan/data/audio/pf-c5.snd"
>     ;n = 1
>     ;g = out 0 (vDiskIn n 0 (sinOsc KR 0.25 0 * 0.25 + 1) Loop)}
> in withSC3 (do {_ <- async (b_alloc 0 8192 n)
>                ;_ <- async (b_read 0 f 0 (-1) 0 True)
>                ;play g })

> withSC3 (do {reset
>             ;_ <- async (b_close 0)
>             ;_ <- async (b_free 0)
>             ;return ()})
