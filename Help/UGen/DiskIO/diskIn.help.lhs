> Sound.SC3.UGen.Help.viewSC3Help "DiskIn"
> Sound.SC3.UGen.DB.ugenSummary "DiskIn"

> import Sound.SC3

> let {f = "/home/rohan/data/audio/pf-c5.snd"
>     ;n = 1
>     ;g = out 0 (diskIn n 0 Loop)}
> in withSC3 (do {_ <- async (b_alloc 0 65536 n)
>                ;_ <- async (b_read 0 f 0 (-1) 0 True)
>                ;play g})

> withSC3 (do {reset
>             ;_ <- async (b_close 0)
>             ;async (b_free 0)})
