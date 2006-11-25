lfNoise1 freq

Ramp noise.  Generates linearly interpolated random values at a
rate given by the nearest integer division of the sample rate by
the freq argument.

freq - approximate rate at which to generate random values.

> audition . (* 0.15) =<< lfNoise1 AR 1000

Modulate frequency.

> let f = xLine KR 1000 10000 10 RemoveSynth
> n <- lfNoise1 AR f
> audition (n * 0.15)

Use as frequency control.

> f <- lfNoise1 KR 4 
> audition $ sinOsc AR (f * 400 + 450) 0 * 0.2
