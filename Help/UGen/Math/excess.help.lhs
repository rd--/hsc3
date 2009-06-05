excess a b

Clipping residual.  Returns the difference of the original signal and
its clipped form: (a - clip2(a,b)).

> import Sound.SC3

> let { o = fSinOsc AR 1000 0
>     ; l = line KR 0 1 8 DoNothing }
> in audition (out 0 (excess o l))

> let { o = fSinOsc AR 1000 0
>     ; l = line KR 0 1 8 DoNothing }
> in audition (out 0 (o - (clip2 o l)))
