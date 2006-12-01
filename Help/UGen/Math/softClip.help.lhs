softClip a

Nonlinear distortion.  Distortion with a perfectly linear region
from -0.5 to +0.5.

> let e = xLine KR 0.1 10 10 DoNothing
>     o = fSinOsc AR 500 0.0
> audition $ softClip (o * e) * 0.25
