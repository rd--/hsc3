absDif a b

Calculates the value of (abs (- a b). Finding the magnitude of the
difference of two values is a common operation.

> import Sound.SC3

> let { p = fSinOsc AR 440 0
>     ; q = fSinOsc AR 2 0 * 0.5 }
> in audition (out 0 (p * absDif 0.2 q))
