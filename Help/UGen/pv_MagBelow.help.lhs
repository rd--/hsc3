> Sound.SC3.UGen.Help.viewSC3Help "PV_MagBelow"
> Sound.SC3.UGen.DB.ugenSummary "PV_MagBelow"

> import Sound.SC3

> let fileName = "/home/rohan/data/audio/pf-c5.snd"
> in withSC3 (do {_ <- async (b_alloc 10 2048 1)
>                ;async (b_allocRead 12 fileName 0 0)})

> let { a = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop DoNothing
>     ; f = fft' 10 a
>     ; x = mouseX KR 0 100 Linear 0.1
>     ; h = pv_MagBelow f x }
> in audition (out 0 (ifft' h * 0.5))

Synthesised input.
> let { a = sinOsc KR (squared (sinOsc KR 0.08 0 * 6 + 6.2)) 0 * 100 + 800
>     ; b = sinOsc AR a 0
>     ; f = fft' 10 b
>     ; x = mouseX KR 0 1024 Linear 0.1
>     ; h = pv_MagBelow f x }
> in audition (out 0 (ifft' h * 0.5))
