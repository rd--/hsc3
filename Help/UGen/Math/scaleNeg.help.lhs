> Sound.SC3.UGen.Help.viewSC3Help "scaleneg"
> :t scaleNeg

> import Sound.SC3

> let {o = fSinOsc AR 1000 0
>     ;l = line AR 1 (-1) 4 RemoveSynth}
> in audition (out 0 (scaleNeg o l))

written out:
> let {o = fSinOsc AR 1000 0
>     ;l = line AR 1 (-1) 4 RemoveSynth
>     ;c = o <* 0}
> in audition (out 0 (c * (o * l) + (1 - c) * o))
