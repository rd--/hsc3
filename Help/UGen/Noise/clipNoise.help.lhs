> Sound.SC3.UGen.Help.viewSC3Help "ClipNoise"
> Sound.SC3.UGen.DB.ugenSummary "ClipNoise"

> import Sound.SC3

> audition . (out 0) . (* 0.1) =<< clipNoiseM AR
> audition . (out 0) . (* 0.1) =<< whiteNoiseM AR
