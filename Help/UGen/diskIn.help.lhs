> Sound.SC3.UGen.Help.viewSC3Help "DiskIn"
> Sound.SC3.UGen.DB.ugenSummary "DiskIn"

> import Sound.SC3

> let {fn = "/home/rohan/data/audio/pf-c5.snd"
>     ;nc = 1
>     ;gr = out 0 (diskIn nc 0 Loop)}
> in withSC3 (do {_ <- async (b_alloc 0 65536 nc)
>                ;_ <- async (b_read 0 fn 0 (-1) 0 True)
>                ;play gr})

> withSC3 (do {reset
>             ;_ <- async (b_close 0)
>             ;async (b_free 0)})
