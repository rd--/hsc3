combN in maxDelayTime delayTime decayTime

Comb delay line. CombN uses no interpolation, CombL uses linear
interpolation, CombC uses all pass interpolation.  All times are in
seconds.  The decay time is the time for the echoes to decay by 60
decibels. If this time is negative then the feedback coefficient
will be negative, thus emphasizing only odd harmonics at an octave
lower.

Comb used as a resonator. The resonant fundamental is equal to
reciprocal of the delay time.

> import Sound.SC3.ID

> let {n = whiteNoise 'a' AR
>     ;dt = xLine KR 0.0001 0.01 20 RemoveSynth}
> in audition (out 0 (combN (n * 0.1) 0.01 dt 0.2))

> let {n = whiteNoise 'a' AR
>     ;dt = xLine KR 0.0001 0.01 20 RemoveSynth}
> in audition (out 0 (combL (n * 0.1) 0.01 dt 0.2))

> let {n = whiteNoise 'a' AR
>     ;dt = xLine KR 0.0001 0.01 20 RemoveSynth}
> in audition (out 0 (combC (n * 0.1) 0.01 dt 0.2))

With negative feedback:

> let {n = whiteNoise 'a' AR
>     ;dt = xLine KR 0.0001 0.01 20 RemoveSynth}
> in audition (out 0 (combC (n * 0.1) 0.01 dt (-0.2)))

Used as an echo.

> let {d = dust 'a' AR 1
>     ;n = whiteNoise 'a' AR
>     ;i = decay (d * 0.5) 0.2 * n}
> in audition (out 0 (combC i 0.2 0.2 3))
