> Sound.SC3.UGen.Help.viewSC3Help "OnePole"
> Sound.SC3.UGen.DB.ugenSummary "OnePole"

> import Sound.SC3

> let n = whiteNoise 'α' AR
> in audition (out 0 (onePole (n * 0.5) 0.95))

> let n = whiteNoise 'α' AR
> in audition (out 0 (onePole (n * 0.5) (-0.95)))

> let {n = whiteNoise 'α' AR
>     ;c = line KR (-0.99) 0.99 10 RemoveSynth}
> in audition (out 0 (onePole (n * 0.5) c))
