> Sound.SC3.UGen.Help.viewSC3Help "PV_BinShift"
> Sound.SC3.UGen.DB.ugenSummary "PV_BinShift"

> import Sound.SC3.ID

> withSC3 (\fd -> async fd (b_alloc 10 2048 1))

> let { x  = mouseX' KR (-10) 100 Linear 0.1
>     ; y  = mouseY' KR 1 4 Linear 0.1
>     ; s0 = sinOsc KR 0.08 0 * 6 + 6.2
>     ; s1 = sinOsc KR (squared s0) 0 * 100 + 800
>     ; s2 = sinOsc AR s1 0
>     ; pv = pv_BinShift (fft' 10 s2) y x }
> in audition (out 0 (pan2 (ifft' pv) 0 0.1))
