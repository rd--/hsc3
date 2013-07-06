> Sound.SC3.UGen.Help.viewSC3Help "PV_BinWipe"
> Sound.SC3.UGen.DB.ugenSummary "PV_BinWipe"

> import Sound.SC3.ID

> let fileName = "/home/rohan/data/audio/pf-c5.snd"
> in withSC3 (do {_ <- async (b_alloc 10 2048 1)
>                ;_ <- async (b_alloc 11 2048 1)
>                ;async (b_allocRead 12 fileName 0 0)})

> let {n = whiteNoise 'α' AR
>     ;b = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop DoNothing
>     ;f = fft' 10 (n * 0.2)
>     ;g = fft' 11 b
>     ;x = mouseX KR 0.0 1.0 Linear 0.1
>     ;h = pv_BinWipe f g x}
> in audition (out 0 (pan2 (ifft' h) 0 0.5))
