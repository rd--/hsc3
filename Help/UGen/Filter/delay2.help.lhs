> Sound.SC3.UGen.Help.viewSC3Help "Delay2"
> Sound.SC3.UGen.DB.ugenSummary "Delay2"

> import Sound.SC3.ID

> let s = impulse AR 1 0
> in audition (out 0 (s + (delay2 s)))
