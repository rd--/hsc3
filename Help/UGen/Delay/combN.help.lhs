combn in maxDelayTime delayTime decayTime

Comb delay line. CombN uses no interpolation, CombL uses linear
interpolation, CombC uses all pass interpolation.  All times are in
seconds.  The decay time is the time for the echoes to decay by 60
decibels. If this time is negative then the feedback coefficient
will be negative, thus emphasizing only odd harmonics at an octave
lower.

Comb used as a resonator. The resonant fundamental is equal to
reciprocal of the delay time.

> combn AR (whitenoise 0 AR * 0.01) 0.01 (xline KR 0.0001 0.01 20 2) 0.2

> combl AR (whitenoise 0 AR * 0.01) 0.01 (xline KR 0.0001 0.01 20 2) 0.2

> combc AR (whitenoise 0 AR * 0.01) 0.01 (xline KR 0.0001 0.01 20 2) 0.2

With negative feedback:

> combn AR (whitenoise 0 AR * 0.01) 0.01 (xline KR 0.0001 0.01 20 2) (-0.2)

> combl AR (whitenoise 0 AR * 0.01) 0.01 (xline KR 0.0001 0.01 20 2) (-0.2)

> combc AR (whitenoise 0 AR * 0.01) 0.01 (xline KR 0.0001 0.01 20 2) (-0.2)

Used as an echo.

> combn AR (decay AR (dust 0 AR 1 * 0.5) 0.2 * (whitenoise 0 AR)) 0.2 0.2 3
