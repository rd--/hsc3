> Sound.SC3.UGen.Help.viewSC3Help "BrownNoise"
> Sound.SC3.UGen.DB.ugenSummary "BrownNoise"

> import Sound.SC3

> let n = brownNoise 'α' AR
> in audition (out 0 (n * 0.1))

> let {n = brownNoise 'α' KR
>     ;o = sinOsc AR (linExp n (-1) 1 64 9600) 0 * 0.1}
> in audition (out 0 o)

Drawings

> import Sound.SC3.Plot {- hsc3-plot -}

> plot_ugen1 0.1 (brownNoise 'α' AR)
