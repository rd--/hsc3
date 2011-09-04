> Sound.SC3.UGen.Help.viewSC3Help "distort"
> :t distort

> import Sound.SC3

> let {e = xLine KR 0.1 10 10 DoNothing
>     ;o = fSinOsc AR 500 0.0}
> in audition (out 0 (distort (o * e) * 0.25))
