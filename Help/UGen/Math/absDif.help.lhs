> Sound.SC3.UGen.Help.viewSC3Help "absdif"
> :t absDif
> import Sound.SC3

Finding the magnitude of the difference of two values is a common operation.
> let {p = fSinOsc AR 440 0
>     ;q = fSinOsc AR 2 0 * 0.5}
> in audition (out 0 (p * absDif 0.2 q))
