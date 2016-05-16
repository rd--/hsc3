> Sound.SC3.UGen.Help.viewSC3Help "Pan2"
> Sound.SC3.UGen.DB.ugenSummary "Pan2"

> import Sound.SC3

> let n = pinkNoise 'α' AR
> in audition (out 0 (pan2 n (fSinOsc KR 2 0) 0.3))

> let {n = pinkNoise 'α' AR
>     ;x = mouseX KR (-1) 1 Linear 0.2
>     ;y = mouseY KR 0 1 Linear 0.2}
> in audition (out 0 (pan2 n x y))
