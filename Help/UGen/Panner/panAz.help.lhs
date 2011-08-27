> Sound.SC3.UGen.Help.viewSC3Help "PanAZ"
> Sound.SC3.UGen.DB.ugenSummary "PanAZ"

> import Sound.SC3.ID

> let {o = pinkNoise 'a' AR
>     ;nc = 4
>     ;fr = 0.15}
> in audition (out 0 (panAz nc o (lfSaw KR fr 0) 0.1 2 0.5))
