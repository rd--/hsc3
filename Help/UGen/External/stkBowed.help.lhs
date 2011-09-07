> Sound.SC3.UGen.Help.viewSC3Help "StkBowed"
> Sound.SC3.UGen.DB.ugenSummary "StkBowed"

> import Sound.SC3

> let g = toggleFF (impulse KR 1 0)
> in audition (out 0 (stkBowed AR 220 64 64 64 64 64 g 1 1))
