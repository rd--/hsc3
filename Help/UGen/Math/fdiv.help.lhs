> Sound.SC3.UGen.Help.viewSC3Help "Operator./"
> :t (/)

> import Sound.SC3

> let { o = fSinOsc KR 10 0.5
>     ; n = pinkNoise 'α' AR }
> in audition (out 0 ((n * 0.1) / (o * 0.75)))
