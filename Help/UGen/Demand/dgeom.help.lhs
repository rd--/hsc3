> Sound.SC3.UGen.Help.viewSC3Help "Dgeom"
> Sound.SC3.UGen.DB.ugenSummary "Dgeom"

> import Sound.SC3.ID

> let {n = dgeom 'α' 15 1 1.2
>     ;x = mouseX KR 1 40 Exponential 0.1
>     ;t = impulse KR x 0
>     ;f = demand t 0 n * 30 + 340}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
