> Sound.SC3.UGen.Help.viewSC3Help "GrayNoise"
> Sound.SC3.UGen.DB.ugenSummary "GrayNoise"

> import Sound.SC3

> audition . (out 0) . (* 0.1) =<< grayNoiseM AR
