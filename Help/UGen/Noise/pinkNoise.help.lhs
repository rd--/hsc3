> Sound.SC3.UGen.Help.viewSC3Help "PinkNoise"
> Sound.SC3.UGen.DB.ugenSummary "PinkNoise"

> import Sound.SC3.Monadic

> audition . (out 0) . (* 0.05) =<< pinkNoise AR
> audition . (out 0) . (* 0.05) =<< whiteNoise AR
> audition . (out 0) . (* 0.05) =<< brownNoise AR
