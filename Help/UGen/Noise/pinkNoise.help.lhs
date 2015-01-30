> Sound.SC3.UGen.Help.viewSC3Help "PinkNoise"
> Sound.SC3.UGen.DB.ugenSummary "PinkNoise"

> import Sound.SC3

> audition . (out 0) . (* 0.05) =<< pinkNoiseM AR
> audition . (out 0) . (* 0.05) =<< whiteNoiseM AR
> audition . (out 0) . (* 0.05) =<< brownNoiseM AR
