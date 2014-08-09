> Sound.SC3.UGen.Help.viewSC3Help "TWChoose"

# tWChoose is a composite of tWindex and select

> import Sound.SC3.ID

> let {x = mouseX KR 1 1000 Exponential 0.1
>     ;d = dust 'a' AR x
>     ;a = mce [sinOsc AR 220 0
>              ,saw AR 440
>              ,pulse AR 110 0.1]
>     ;w = mce [0.5, 0.35, 0.15]
>     ;o = tWChoose 'b' d a w 0}
> in audition (out 0 (o * 0.1))
