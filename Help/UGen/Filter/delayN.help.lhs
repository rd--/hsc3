delayN in maxDelayTime delayTime

Simple delay line.  There are three forms, delayN uses no
interpolation, delayL uses linear interpolation, delayA uses
all pass interpolation.  The maximum delay length is set at
initialization time and cannot be extended.

Dust randomly triggers Decay to create an exponential decay
envelope for the WhiteNoise input source.  The input is
mixed with the delay.

> d <- dust AR 1
> n <- whiteNoise AR
> let z = decay d 0.3 * n
> audition (out 0 (z + delayN z 0.2 0.2))
