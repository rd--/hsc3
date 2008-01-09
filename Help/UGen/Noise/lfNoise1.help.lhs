lfNoise1 rate freq

Ramp noise.  Generates linearly interpolated random values at a
rate given by the nearest integer division of the sample rate by
the freq argument.

freq - approximate rate at which to generate random values.

> audition . (out 0) . (* 0.05) =<< lfNoise1 AR 1000

Modulate frequency.

> let f = xLine KR 1000 10000 10 RemoveSynth
> in do { n <- lfNoise1 AR f
>       ; audition (out 0 (n * 0.05)) }

Use as frequency control.

> do { n <- lfNoise1 KR 4 
>    ; let f = n * 400 + 450
>      in audition (out 0 (sinOsc AR f 0 * 0.1)) }
