> Sound.SC3.UGen.Help.viewSC3Help "PV_MagFreeze"
> Sound.SC3.UGen.DB.ugenSummary "PV_MagFreeze"

> import Sound.SC3.ID {- hsc3 -}

Load audio file.

> let fileName = "/home/rohan/data/audio/pf-c5.snd"
> in withSC3 (do {_ <- async (b_alloc 10 2048 1)
>                ;async (b_allocRead 12 fileName 0 0)})

File as signal...

> let z = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop DoNothing

Synthesised signal...

> let z = let {o1 = sinOsc KR 0.08 0
>             ;o2 = sinOsc KR (squared (o1 * 6 + 6.2)) 0 * 100 + 800}
>         in sinOsc AR o2 0

Outside world signal...

> let z = soundIn 4

Process (freeze) 'z'...

> let {f = fft' 10 z
>     ;x = mouseX KR 0 1 Linear 0.1
>     ;h = pv_MagFreeze f (x >* 0.5)}
> in audition (out 0 (ifft' h * 0.5))
