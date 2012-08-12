> Sound.SC3.UGen.Help.viewSC3Help "TPV"
> Sound.SC3.UGen.DB.ugenSummary "TPV"

> import Sound.SC3

> let fn = "/home/rohan/data/audio/pf-c5.snd"
> in withSC3 (do {_ <- async (b_alloc 0 2048 1)
>                ;async (b_allocRead 1 fn 0 0)})

> let {i = playBuf 1 AR 1 (bufRateScale KR 1) 1 0 Loop DoNothing
>     ;f = fft 0 i 0.5 1 1 0
>     ;x = mouseX KR 1 70 Linear 0.1
>     ;y = mouseX KR 0.25 3 Linear 0.1
>     ;o = tpv f 1024 512 70 x y 4 0.2}
> in audition (out 0 (pan2 o 0 1))
