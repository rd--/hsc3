> Sound.SC3.UGen.Help.viewSC3Help "PV_RectComb"
> Sound.SC3.UGen.DB.ugenSummary "PV_RectComb"

> import Sound.SC3

> withSC3 (async (b_alloc 10 2048 1))

noise source

> let z = whiteNoise 'α' AR * 0.3

outside world

> let z = soundIn 4

> let {x = mouseX KR 0 0.5 Linear 0.1
>     ;y = mouseY KR 0 0.5 Linear 0.1
>     ;c = pv_RectComb (fft' 10 z) 8 x y}
> in audition (out 0 (pan2 (ifft' c) 0 1))

> let {p = lfTri KR 0.097 0 *   0.4  + 0.5
>     ;w = lfTri KR 0.240 0 * (-0.5) + 0.5
>     ;c = pv_RectComb (fft' 10 z) 8 p w}
> in audition (out 0 (pan2 (ifft' c) 0 1))
