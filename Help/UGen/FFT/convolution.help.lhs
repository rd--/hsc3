> Sound.SC3.UGen.Help.viewSC3Help "Convolution"
> Sound.SC3.UGen.DB.ugenSummary "Convolution"

> import Sound.SC3.ID

> let {k = whiteNoise 'a' AR
>     ;i = in' 2 AR numOutputBuses}
> in audition (out 0 (convolution i k 2048 * 0.1))
