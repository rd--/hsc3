> Sound.SC3.UGen.Help.viewSC3Help "Limiter"
> Sound.SC3.UGen.DB.ugenSummary "Limiter"

> import Sound.SC3

example signal
> let z = let i = impulse AR 8 0 * lfSaw KR 0.25 0 * (-0.6) + 0.7
>         in decay2 i 0.001 0.3 * fSinOsc AR 500 0

unprocessed
> audition (out 0 z)

limited
> audition (out 0 (limiter z 0.4 0.01))
