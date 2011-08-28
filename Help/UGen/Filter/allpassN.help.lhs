> Sound.SC3.UGen.Help.viewSC3Help "AllpassN"
> Sound.SC3.UGen.DB.ugenSummary "AllpassN"

> import Sound.SC3.ID

Since the allpass delay has no audible effect as a resonator on steady
state sound ...
> let {dly = xLine KR 0.0001 0.01 20 RemoveSynth
>     ;n = whiteNoise 'a' AR}
> in audition (out 0 (allpassC (n * 0.1) 0.01 dly 0.2))

...these examples add the input to the effected sound so that you
can hear the effect of the phase comb.
> let {n = whiteNoise 'a' AR
>     ;dly = xLine KR 0.0001 0.01 20 RemoveSynth}
> in audition (out 0 ((n + allpassN (n * 0.1) 0.01 dly 0.2) * 0.1))

Linear variant
> let {n = whiteNoise 'a' AR
>     ;dly = xLine KR 0.0001 0.01 20 RemoveSynth}
> in audition (out 0 ((n + allpassL (n * 0.1) 0.01 dly 0.2) * 0.1))

Cubic variant
> let {n = whiteNoise 'a' AR
>     ;dly = xLine KR 0.0001 0.01 20 RemoveSynth}
> in audition (out 0 ((n + allpassC (n * 0.1) 0.01 dly 0.2) * 0.1))

Used as an echo - doesn't really sound different than Comb, but it
outputs the input signal immediately (inverted) and the echoes are
lower in amplitude.
> let {n = whiteNoise 'a' AR
>     ;d = dust 'a' AR 1
>     ;src = decay (d * 0.5) 0.2 * n}
> in audition (out 0 (allpassN src 0.2 0.2 3))
