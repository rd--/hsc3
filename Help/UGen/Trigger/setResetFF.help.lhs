> Sound.SC3.UGen.Help.viewSC3Help "SetResetFF"
> Sound.SC3.UGen.DB.ugenSummary "SetResetFF"

> import Sound.SC3.ID

d0 is the set trigger, d1 the reset trigger
> let {n = brownNoise 'α' AR
>     ;d0 = dust 'α' AR 5
>     ;d1 = dust 'β' AR 5}
> in audition (out 0 (setResetFF d0 d1 * n * 0.2))
