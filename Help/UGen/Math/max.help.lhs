max a b

Maximum.

> import Sound.SC3

> let { p = fSinOsc AR 500 0 * 0.25
>     ; q = fSinOsc AR 0.5 0 }
> in audition (out 0 (p `max` q))
