lfNoise1 rate freq

Ramp noise.  Generates linearly interpolated random values at a
rate given by the nearest integer division of the sample rate by
the freq argument.

freq - approximate rate at which to generate random values.

> import Sound.SC3.ID

> audition (out 0 (lfNoise1 'a' AR 1000 * 0.05))

Modulate frequency.

> let { f = xLine KR 1000 10000 10 RemoveSynth
>     ; n = lfNoise1 'a' AR f }
> in audition (out 0 (n * 0.05))

Use as frequency control.

> let { n = lfNoise1 'a' KR 4
>     ; f = n * 400 + 450 }
> in audition (out 0 (sinOsc AR f 0 * 0.1))
