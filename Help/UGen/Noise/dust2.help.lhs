> Sound.SC3.UGen.Help.viewSC3Help "Dust2"
> Sound.SC3.UGen.DB.ugenSummary "Dust2"

> import Sound.SC3

> let n = dust2 'α' AR 200
> in audition (out 0 (n * 0.5))

> let d = xLine KR 20000 2 10 RemoveSynth
> in audition (out 0 (dust2 'β' AR d * 0.15))

Drawing

> import Sound.SC3.Plot {- hsc3-plot -}

> plot_ugen1 0.1 (dust2 'γ' AR 400)
> plot_ugen1 0.1 (dust2 'γ' AR (xLine KR 5000 1 0.1 RemoveSynth))
