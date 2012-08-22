> Sound.SC3.UGen.Help.viewSC3Help "Dwrand"
> Sound.SC3.UGen.DB.ugenSummary "Dwrand"

> import Sound.SC3.ID

> let {n = dwrand 'a' dinf (mce [1,3,2,7,8]) (mce [0.4,0.4,0.05,0.05,0.1])
>     ;x = mouseX KR 1 400 Exponential 0.1
>     ;t = impulse KR x 0
>     ;f = demand t 0 n * 30 + 340}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
