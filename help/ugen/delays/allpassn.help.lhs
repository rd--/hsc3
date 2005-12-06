allpassn in maxDelayTime delayTime decayTime

All pass delay line. AllpassN uses no interpolation, AllpassL uses
linear interpolation, AllpassC uses all pass interpolation.  All
time values are in seconds.  The decay time is the time for the
echoes to decay by 60 decibels. If this time is negative then the
feedback coefficient will be negative, thus emphasizing only odd
harmonics at an octave lower.

Since the allpass delay has no audible effect as a resonator on
steady state sound ...

> allpassc AR (whitenoise 0 AR * 0.1) 0.01 (xline KR 0.0001 0.01 20 2) 0.2

...these examples add the input to the effected sound so that you
can hear the effect of the phase comb.

> let z = whitenoise 0 AR 0.1
> in z + allpassn AR z 0.01 (xline KR 0.0001 0.01 20 2) 0.2

> let z = whitenoise 0 AR 0.1
> in z + allpassl AR z 0.01 (xline KR 0.0001 0.01 20 2) 0.2

> let z = whitenoise 0 AR 0.1
> in z + allpassc AR z 0.01 (xline KR 0.0001 0.01 20 2) 0.2

Used as an echo - doesn't really sound different than Comb, but it
outputs the input signal immediately (inverted) and the echoes are
lower in amplitude.

> allpassn AR (decay AR (dust 0 AR 1 * 0.5) 0.2 * whitenoise 0 AR) 0.2 0.2 3
