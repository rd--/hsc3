> Sound.SC3.UGen.Help.viewSC3Help "LocalOut"
> Sound.SC3.UGen.DB.ugenSummary "LocalOut"

> import Sound.SC3.ID

Resonator, must subtract blockSize for correct tuning
> let {p = localIn 1 AR
>     ;i = impulse AR 1 0
>     ;d = delayC (i + (p * 0.995)) 1 (recip 440 - recip controlRate)}
> in audition (mrg [offsetOut 0 p,localOut d])

Compare with oscillator.
> audition (out 1 (sinOsc AR 440 0 * 0.2))
