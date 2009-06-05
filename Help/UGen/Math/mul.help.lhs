a * b

Multiplication.

> import Sound.SC3

> audition (out 0 (sinOsc AR 440 0 * 0.5))

Creates a beating effect (subaudio rate).

> do { n <- pinkNoise AR
>    ; audition (out 0 (fSinOsc kr 10 0 * n * 0.5)) }

Ring modulation.

> let { p = sinOsc AR (xLine KR 100 1001 10 DoNothing) 0
>     ; q = syncSaw AR 100 200 }
> in audition (out 0 (p * q * 0.25))
