distort a

Nonlinear distortion.

> let e = xLine KR 0.1 10 10 DoNothing
>     o = fSinOsc AR 500 0.0
> audition $ distort (o * e) * 0.25
