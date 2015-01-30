> Sound.SC3.UGen.Help.viewSC3Help "PV_Diffuser"
> Sound.SC3.UGen.DB.ugenSummary "PV_Diffuser"

> import Sound.SC3

> let fileName = "/home/rohan/data/audio/pf-c5.snd"
> in withSC3 (do {_ <- async (b_alloc 10 2048 1)
>                ;async (b_allocRead 12 fileName 0 0)})

> let z = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop DoNothing

> let z = soundIn 4

> let {f = fft' 10 z
>     ;x = mouseX KR 0 1 Linear 0.1
>     ;h = pv_Diffuser f (x >* 0.5) }
> in audition (out 0 (ifft' h * 0.5))
