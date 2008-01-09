allpassN in maxDelayTime delayTime decayTime

All pass delay line. AllpassN uses no interpolation, AllpassL uses
linear interpolation, AllpassC uses all pass interpolation.  All time
values are in seconds.  The decay time is the time for the echoes to
decay by 60 decibels. If this time is negative then the feedback
coefficient will be negative, thus emphasizing only odd harmonics at
an octave lower.

Since the allpass delay has no audible effect as a resonator on steady
state sound ...

> let dly = xLine KR 0.0001 0.01 20 RemoveSynth
> in do { n <- whiteNoise AR
>       ; audition (out 0 (allpassC (n * 0.1) 0.01 dly 0.2)) }

...these examples add the input to the effected sound so that you
can hear the effect of the phase comb.

> do { n <- whiteNoise AR
>    ; let dly = xLine KR 0.0001 0.01 20 RemoveSynth
>      in audition (out 0 ((n + allpassN (n * 0.1) 0.01 dly 0.2) * 0.1)) }

Linear variant

> do { n <- whiteNoise AR
>    ; let dly = xLine KR 0.0001 0.01 20 RemoveSynth
>      in audition (out 0 ((n + allpassL (n * 0.1) 0.01 dly 0.2) * 0.1)) }

Cubic variant

> do { n <- whiteNoise AR
>    ; let dly = xLine KR 0.0001 0.01 20 RemoveSynth
>      in audition (out 0 ((n + allpassC (n * 0.1) 0.01 dly 0.2) * 0.1)) }

Used as an echo - doesn't really sound different than Comb, but it
outputs the input signal immediately (inverted) and the echoes are
lower in amplitude.

> do { n <- whiteNoise AR
>    ; d <- dust AR 1
>    ; let src = decay (d * 0.5) 0.2 * n
>      in audition (out 0 (allpassN src 0.2 0.2 3)) }
