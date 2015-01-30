> Sound.SC3.UGen.Help.viewSC3Help "LinLin"
> Sound.SC3.UGen.DB.ugenSummary "LinLin"

> import Sound.SC3

> let f = linLin (mouseX KR 0 1 Linear 0.2) 0 1 440 660
> in audition (out 0 (sinOsc AR f 0 * 0.1))

The destination range may be k-rate.

> let { x = mouseX KR 0 1 Linear 0.2
>     ; y = mouseY KR 220 440 Linear 0.2
>     ; f = linLin x 0 1 y 660 }
> in audition (out 0 (sinOsc AR f 0 * 0.1))

Modulating source and destination values.

> let {n = lfNoise2 'α' AR 80
>     ;x = mouseX KR 200 8000 Linear 0.2
>     ;y = mouseY KR 200 8000 Linear 0.2
>     ;f = linLin n (sinOsc KR 0.2 0) (sinOsc KR 0.2543 0) x y}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
