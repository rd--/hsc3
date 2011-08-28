> Sound.SC3.UGen.Help.viewSC3Help "Dust2"
> Sound.SC3.UGen.DB.ugenSummary "Dust2"

> import Sound.SC3.ID

> let n = dust2 'a' AR 200
> in audition (out 0 (n * 0.5))

> let d = xLine KR 20000 2 10 RemoveSynth
> in audition (out 0 (dust2 'b' AR d * 0.15))
