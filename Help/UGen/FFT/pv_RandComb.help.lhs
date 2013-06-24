> Sound.SC3.UGen.Help.viewSC3Help "PV_RandComb"
> Sound.SC3.UGen.DB.ugenSummary "PV_RandComb"

> import Sound.SC3.ID

allocate buffer
> withSC3 (async (b_alloc 10 2048 1))

noise signal
> let z = whiteNoise 'a' AR * 0.5

outside world
> let z = soundIn 4

processor
> let {t = impulse KR 0.1 0
>     ;x = mouseX KR 0.6 0.95 Linear 0.1
>     ;c = pv_RandComb 'a' (fft' 10 z) x t}
> in audition (out 0 (pan2 (ifft' c) 0 1))
