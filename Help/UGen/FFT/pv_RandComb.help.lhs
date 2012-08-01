> Sound.SC3.UGen.Help.viewSC3Help "PV_RandComb"
> Sound.SC3.UGen.DB.ugenSummary "PV_RandComb"

> import Sound.SC3.ID

> withSC3 (async (b_alloc 10 2048 1))

> let {x = mouseX KR 0.6 0.95 Linear 0.1
>     ;t = impulse KR 0.4 0
>     ;n = whiteNoise 'a' AR
>     ;c = pv_RandComb 'a' (fft' 10 (n * 0.5)) x t}
> in audition (out 0 (pan2 (ifft' c) 0 1))
