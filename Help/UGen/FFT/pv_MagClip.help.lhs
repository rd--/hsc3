> Sound.SC3.UGen.Help.viewSC3Help "PV_MagClip"
> Sound.SC3.UGen.DB.ugenSummary "PV_MagClip"

> import Sound.SC3.ID

> let fileName = "/home/rohan/data/audio/pf-c5.snd"
> in withSC3 (do {_ <- async (b_alloc 10 2048 1)
>                ;async (b_allocRead 12 fileName 0 0)})

File input
> let z = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop DoNothing

Synthesised input.
> let z = let {f0 = squared (sinOsc KR 0.08 0 * 6 + 6.2)
>             ;f1 = sinOsc KR f0 0 * 100 + 800}
>         in sinOsc AR f1 0

Outside world
> let z = soundIn 4

> let {f = fft' 10 z
>     ;c = 128
>     ;x = mouseX KR 0 c Linear 0.1
>     ;h = pv_MagClip f x}
> in audition (out 0 (ifft' h * 0.5))
