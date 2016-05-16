> Sound.SC3.UGen.Help.viewSC3Help "Operator.fold2"
> :t fold2

> import Sound.SC3

> let { o = fSinOsc AR 1000 0
>     ; l = line KR 0 1 8 DoNothing }
> in audition (out 0 (fold2 o l))
