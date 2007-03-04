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
> n <- whiteNoise AR
> audition $ allpassC (n * 0.1) 0.01 dly 0.2

...these examples add the input to the effected sound so that you
can hear the effect of the phase comb.

> n <- whiteNoise AR
> let dly = xLine KR 0.0001 0.01 20 RemoveSynth
> audition $ (n + allpassN (n * 0.1) 0.01 dly 0.2) * 0.1

Linear variant

> n <- whiteNoise AR
> let dly = xLine KR 0.0001 0.01 20 RemoveSynth
> audition $ (n + allpassL (n * 0.1) 0.01 dly 0.2) * 0.1

Cubic variant

> n <- whiteNoise AR
> let dly = xLine KR 0.0001 0.01 20 RemoveSynth
> audition $ (n + allpassC (n * 0.1) 0.01 dly 0.2) * 0.1

Used as an echo - doesn't really sound different than Comb, but it
outputs the input signal immediately (inverted) and the echoes are
lower in amplitude.

> n <- whiteNoise AR
> d <- dust AR 1
> let src = decay (d * 0.5) 0.2 * n
> audition $ allpassN src 0.2 0.2 3
