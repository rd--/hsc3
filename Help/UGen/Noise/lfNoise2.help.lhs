lfNoise2 freq

Quadratic noise.  Generates quadratically interpolated random
values at a rate given by the nearest integer division of the
sample rate by the freq argument.

> audition . (out 0) . (* 0.15) =<< lfNoise2 AR 1000

Modulate frequency.

> let f = xLine KR 1000 10000 10 RemoveSynth
> n <- lfNoise2 AR f
> audition (out 0 (n * 0.15))

Use as frequency control.

> f <- lfNoise2 KR 4 
> audition (out 0 (sinOsc AR (f * 400 + 450) 0 * 0.2))
