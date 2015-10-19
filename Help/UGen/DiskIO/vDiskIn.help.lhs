> Sound.SC3.UGen.Help.viewSC3Help "VDiskIn"
> Sound.SC3.UGen.DB.ugenSummary "VDiskIn"

> import Sound.SC3 {- hsc3 -}

> let {fn = "/home/rohan/data/audio/pf-c5.snd"
>     ;nc = 1
>     ;gr = out 0 (vDiskIn nc 0 (sinOsc KR 0.25 0 * 0.25 + 1) Loop 0)}
> in withSC3 (do {_ <- async (b_alloc 0 8192 nc)
>                ;_ <- async (b_read 0 fn 0 (-1) 0 True)
>                ;play gr})

> withSC3 (do {reset
>             ;_ <- async (b_close 0)
>             ;_ <- async (b_free 0)
>             ;return ()})
