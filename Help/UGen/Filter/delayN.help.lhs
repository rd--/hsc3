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
>     x = mouseX KR 0.0 0.2 Linear 0.1
> audition (out 0 (z + delayN z 0.2 x))

The delay time can be varied at control rate.
An oscillator either reinforcing or cancelling
with the delayed copy of itself.

> let o = sinOsc AR 320 0 * 0.1
>     l = 0.005
>     x = mouseX KR 0.0 l Linear 0.15
> audition (out 0 (o + delayN o l x))
