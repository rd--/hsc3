> Sound.SC3.UGen.Help.viewSC3Help "TwoPole"
> Sound.SC3.UGen.DB.ugenSummary "TwoPole"

> import Sound.SC3.ID

> let n = whiteNoise 'a' AR
> in audition (out 0 (twoPole (n * 0.005) 2000 0.95))

> let {n = whiteNoise 'a' AR
>     ;f = xLine KR 800 8000 8 RemoveSynth}
> in audition (out 0 (twoPole (n * 0.005) f 0.95))
