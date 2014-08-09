> Sound.SC3.UGen.Help.viewSC3Help "Dwrand"
> Sound.SC3.UGen.DB.ugenSummary "Dwrand"

> import Sound.SC3

> let {n = dwrand 'α' dinf (mce [0.3,0.2,0.1,0.2,0.2]) (mce [1,3,2,7,8])
>     ;x = mouseX KR 1 400 Exponential 0.1
>     ;t = impulse KR x 0
>     ;f = demand t 0 n * 30 + 340}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
