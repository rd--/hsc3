> Sound.SC3.UGen.Help.viewSC3Help "LinExp"
> Sound.SC3.UGen.DB.ugenSummary "LinExp"

> let f = linExp (mouseX' KR 0 1 Linear 0.2) 0 1 440 660
> in audition (out 0 (sinOsc AR f 0 * 0.1))

The destination range may be k-rate.
> let {x = mouseX' KR 0 1 Linear 0.2
>     ;y = mouseY' KR 220 440 Linear 0.2
>     ;f = linExp x 0 1 y 660}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
