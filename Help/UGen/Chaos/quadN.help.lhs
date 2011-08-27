> Sound.SC3.UGen.Help.viewSC3Help "QuadN"
> Sound.SC3.UGen.DB.ugenSummary "QuadN"

> import Sound.SC3

> audition (out 0 (quadC AR 4000 1 (-1) (-0.75) 0 * 0.2))

> let x = mouseX' KR 3.5441 4 Linear 0.1
> in audition (out 0 (quadC AR 4000 (negate x) x 0 0.1 * 0.4))

> let {x = mouseX' KR 3.5441 4 Linear 0.1
>     ;f = quadC AR 4 (negate x) x 0 0.1 * 800 + 900}
> in audition (out 0 (sinOsc AR f 0 * 0.4))
