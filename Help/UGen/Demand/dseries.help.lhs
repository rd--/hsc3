> Sound.SC3.UGen.Help.viewSC3Help "Dseries"
> Sound.SC3.UGen.DB.ugenSummary "Dseries"

> import Sound.SC3.ID

> let {n = dseries 'α' 15 0 1
>     ;x = mouseX KR 1 40 Exponential 0.1
>     ;t = impulse KR x 0
>     ;f = demand t 0 n * 30 + 340}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
