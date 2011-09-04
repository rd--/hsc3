> Sound.SC3.UGen.Help.viewSC3Help "OneZero"
> Sound.SC3.UGen.DB.ugenSummary "OneZero"

> import Sound.SC3.ID

> let n = whiteNoise 'a' AR
> in audition (out 0 (oneZero (n * 0.5) 0.5))

> let n = whiteNoise 'a' AR
> in audition (out 0 (oneZero (n * 0.5) (-0.5)))

> let {n = whiteNoise 'a' AR
>     ;c = line KR (-0.5) 0.5 10 RemoveSynth}
> in audition (out 0 (oneZero (n * 0.5) c))
