> Sound.SC3.UGen.Help.viewSC3Help "Operator.excess"
> :t excess

> import Sound.SC3

> let { o = fSinOsc AR 1000 0
>     ; l = line KR 0 1 8 DoNothing }
> in audition (out 0 (excess o l))

or written out in terms of clip2
> let { o = fSinOsc AR 1000 0
>     ; l = line KR 0 1 8 DoNothing }
> in audition (out 0 (o - (clip2 o l)))
