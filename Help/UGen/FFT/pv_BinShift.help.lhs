> Sound.SC3.UGen.Help.viewSC3Help "PV_BinShift"
> Sound.SC3.UGen.DB.ugenSummary "PV_BinShift"

> import Sound.SC3

allocate buffer

> withSC3 (async (b_alloc 10 2048 1))

source signal (oscillators)

> let z = let {s0 = sinOsc KR 0.08 0 * 6 + 6.2
>             ;s1 = sinOsc KR (squared s0) 0 * 100 + 800}
>         in sinOsc AR s1 0 * 0.2

source signal (the world)

> let z = soundIn 4

default values

> audition (out 0 (ifft' (pv_BinShift (fft' 10 z) 1 0 0)))

mouse control

> let {x = mouseX KR (-10) 100 Linear 0.1
>     ;y = mouseY KR 1 4 Linear 0.1
>     ;b = mouseButton KR 0 1 0.2
>     ;pv = pv_BinShift (fft' 10 z) y x b}
> in audition (out 0 (ifft' pv))
