> Sound.SC3.UGen.Help.viewSC3Help "GrayNoise"
> Sound.SC3.UGen.DB.ugenSummary "GrayNoise"

> import Sound.SC3
> import qualified Sound.SC3.Monadic as M

> audition . (out 0) . (* 0.1) =<< M.grayNoise AR
