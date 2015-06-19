> Sound.SC3.UGen.Help.viewSC3Help "GrayNoise"
> Sound.SC3.UGen.DB.ugenSummary "GrayNoise"

> import Sound.SC3

> audition . (out 0) . (* 0.1) =<< grayNoiseM AR

Drawing

> import Sound.SC3.Plot {- hsc3-plot -}

> plot_ugen1 0.025 (grayNoise 'γ' AR)
