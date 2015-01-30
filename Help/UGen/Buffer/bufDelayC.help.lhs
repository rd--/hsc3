> Sound.SC3.UGen.Help.viewSC3Help "BufDelayC"
> Sound.SC3.UGen.DB.ugenSummary "BufDelayC"

> import Sound.SC3

Allocate buffer zero (required for examples below)

> withSC3 (async (b_alloc 0 44100 1))

Dust randomly triggers Decay to create an exponential decay envelope
for the WhiteNoise input source.  The input is mixed with the delay.

> let {t = dust 'α' AR 1
>     ;n = whiteNoise 'β' AR
>     ;d = decay t 0.5 * n * 0.3}
> in audition (out 0 (bufDelayC 0 d 0.2 + d))

Mouse control for delay time

> let {t = dust 'α' AR 1
>     ;n = whiteNoise 'β' AR
>     ;d = decay t 0.3 * n
>     ;x = mouseX KR 0.0 0.2 Linear 0.1}
> in audition (out 0 (d + bufDelayC 0 d x))
