> Sound.SC3.UGen.Help.viewSC3Help "OffsetOut"
> Sound.SC3.UGen.DB.ugenSummary "OffsetOut"

> import Sound.SC3

> let {a = offsetOut 0 (impulse AR 5 0)
>     ;b = out 0 (sinOsc AR 60 0 * 0.1)}
> in audition (mrg [a,b])

> let {a = out 0 (impulse AR 5 0)
>     ;b = out 0 (sinOsc AR 60 0 * 0.1) }
> in audition (mrg [a,b])
