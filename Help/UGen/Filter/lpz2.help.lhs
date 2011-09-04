> Sound.SC3.UGen.Help.viewSC3Help "LPZ2"
> Sound.SC3.UGen.DB.ugenSummary "LPZ2"

> import Sound.SC3.ID

> let n = whiteNoise 'a' AR
> in audition (out 0 (lpz2 (n * 0.25)))
