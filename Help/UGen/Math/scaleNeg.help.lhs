scaleNeg a b

Scale negative part of input wave.  a * b when a < 0, otherwise a.

> import Sound.SC3

> let { o = fSinOsc AR 1000 0
>     ; l = line AR 1 (-1) 4 RemoveSynth }
> in audition (out 0 (scaleNeg o l))
