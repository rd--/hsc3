> Sound.SC3.UGen.Help.viewSC3Help "LPZ1"
> Sound.SC3.UGen.DB.ugenSummary "LPZ1"

> import Sound.SC3.ID

> let n = whiteNoise 'a' AR * 0.1
> in audition (out 0 (mce2 n (lpz1 n)))
