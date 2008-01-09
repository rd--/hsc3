lfClipNoise rate freq

Randomly generates the values -1 or +1 at a rate given by the
nearest integer division of the sample rate by the freq argument.
It is probably pretty hard on your speakers.  The freq argument is
the approximate rate at which to generate random values.

> audition . (out 0) . (* 0.05) =<< lfClipNoise AR 1000

Modulate frequency

> let f = xLine KR 1000 10000 10 RemoveSynth
> in audition . (out 0) . (* 0.05) =<< lfClipNoise AR f

Use as frequency control

> do { n <- lfClipNoise KR 4 
>    ; audition (out 0 (sinOsc AR (n * 200 + 600) 0 * 0.1)) }
