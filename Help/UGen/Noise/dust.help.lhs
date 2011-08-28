> Sound.SC3.UGen.Help.viewSC3Help "Dust"
> Sound.SC3.UGen.DB.ugenSummary "Dust"

> import Sound.SC3.ID

> audition (out 0 (dust 'a' AR 200 * 0.25))

> let d = xLine KR 20000 2 10 RemoveSynth
> in audition (out 0 (dust 'a' AR d * 0.15))

Illustrate monadic constructor
> import qualified Sound.SC3.Monadic as M

> audition . (out 0) . (* 0.25) =<< M.dust AR 200

> let d = xLine KR 20000 2 10 RemoveSynth
> in audition . (out 0) . (* 0.15) =<< M.dust AR d
