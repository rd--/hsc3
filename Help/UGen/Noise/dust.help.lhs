> Sound.SC3.UGen.Help.viewSC3Help "Dust"
> Sound.SC3.UGen.DB.ugenSummary "Dust"

> import Sound.SC3

> audition (out 0 (dust 'α' AR 200 * 0.25))

> let d = xLine KR 20000 2 10 RemoveSynth
> in audition (out 0 (dust 'β' AR d * 0.15))

Drawing

> import Sound.SC3.Plot {- hsc3-plot -}

> plot_ugen1 0.1 (dust 'γ' AR 300)
> plot_ugen1 0.1 (dust 'γ' AR (xLine KR 5000 1 0.1 RemoveSynth))

Illustrate monadic constructor

> audition . (out 0) . (* 0.25) =<< dustM AR 200

> let d = xLine KR 20000 2 10 RemoveSynth
> in audition . (out 0) . (* 0.15) =<< dustM AR d
