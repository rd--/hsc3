> Sound.SC3.UGen.Help.viewSC3Help "Normalizer"
> Sound.SC3.UGen.DB.ugenSummary "Normalizer"

> import Sound.SC3

> let {s = fSinOsc AR 500 0
>     ;t = impulse AR 8 (lfSaw KR 0.25 (-0.6) * 0.7)
>     ;z = decay2 t 0.001 0.3 * s}
> in audition (out 0 (mce [z, normalizer z 0.4 0.01]))
