> Sound.SC3.UGen.Help.viewSC3Help "DiskIn"
> Sound.SC3.UGen.DB.ugenSummary "DiskIn"

> import Sound.SC3

> let {f = "/home/rohan/audio/metal.wav"
>     ;n = 1
>     ;g = out 0 (diskIn n 0 Loop)}
> in withSC3 (\fd -> do {async fd (b_alloc 0 8192 n)
>                       ;async fd (b_read 0 f 0 (-1) 0 1)
>                       ;play fd g})

> withSC3 (\fd -> do {reset fd
>                    ;async fd (b_close 0)
>                    ;async fd (b_free 0)})
