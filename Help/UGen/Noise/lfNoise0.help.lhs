lfNoise0 freq

Step noise.  Generates random values at a rate given by the nearest
integer division of the sample rate by the freq argument.

> audition . (* 0.15) =<< lfNoise0 AR 1000

Modulate frequency.

> let f = xLine KR 1000 10000 10 RemoveSynth
> n <- lfNoise0 AR f
> audition (n * 0.15)

Use as frequency control.

> f <- lfNoise0 KR 4 
> audition $ sinOsc AR (f * 400 + 450) 0 * 0.2
