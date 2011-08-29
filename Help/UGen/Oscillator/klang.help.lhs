> Sound.SC3.UGen.Help.viewSC3Help "Klang"
> Sound.SC3.UGen.DB.ugenSummary "Klang"

# SC3
Input re-ordering of specification array.

> import Sound.SC3

> let {f = [440,550..1100]
>     ;a = take 7 (cycle [0.05, 0.02])
>     ;p = replicate 7 0}
> in audition (out 0 (klang AR 1 0 (klangSpec f a p)))
