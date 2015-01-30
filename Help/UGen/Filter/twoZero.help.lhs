> Sound.SC3.UGen.Help.viewSC3Help "TwoZero"
> Sound.SC3.UGen.DB.ugenSummary "TwoZero"

> import Sound.SC3

> let {n = whiteNoise 'α' AR
>     ;f = xLine KR 20 20000 8 RemoveSynth}
> in audition (out 0 (twoZero (n * 0.125) f 1))
