delayn in maxDelayTime delayTime

Simple delay line.  There are three forms, DelayN uses no
interpolation, DelayL uses linear interpolation, DelayA uses all
pass interpolation.  The maximum delay length is set at
initialization time and cannot be extended.

Dust randomly triggers Decay to create an exponential decay
envelope for the WhiteNoise input source.  The input is mixed with
delay via the add input.

> let z = decay AR (dust 0 AR 1) 0.3 * whitenoise 0 AR
> in z + delayn AR z 0.2 0.2
