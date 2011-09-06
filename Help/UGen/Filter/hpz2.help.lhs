> Sound.SC3.UGen.Help.viewSC3Help "HPZ2"
> Sound.SC3.UGen.DB.ugenSummary "HPZ2"

> import Sound.SC3.ID

> let n = whiteNoise 'a' AR
> in audition (out 0 (hpz2 (n * 0.25)))
