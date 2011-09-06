> Sound.SC3.UGen.Help.viewSC3Help "ClipNoise"
> Sound.SC3.UGen.DB.ugenSummary "ClipNoise"

> import Sound.SC3
> import qualified Sound.SC3.Monadic as M

> audition . (out 0) . (* 0.1) =<< M.clipNoise AR
> audition . (out 0) . (* 0.1) =<< M.whiteNoise AR
