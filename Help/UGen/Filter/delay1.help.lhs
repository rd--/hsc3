> Sound.SC3.UGen.Help.viewSC3Help "Delay1"
> Sound.SC3.UGen.DB.ugenSummary "Delay1"

> let s = impulse AR 1 0
> in audition (out 0 (s + (delay1 s)))

original, subtract delayed from original
> let z = dust 'a' AR 1000
> in audition (out 0 (mce2 z (z - delay1 z)))
