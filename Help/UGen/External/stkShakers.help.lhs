> Sound.SC3.UGen.Help.viewSC3Help "StkShakers"
> Sound.SC3.UGen.DB.ugenSummary "StkShakers"

> import Control.Monad
> import Sound.SC3

> let {x = mouseX KR 0.25 4 Linear 0.2
>     ;tr = impulse KR x 0 - 0.5}
> in do {i <- tRandM 0 23 tr
>       ;[e,sd,no,rf] <- replicateM 4 (tRandM 0 127 tr)
>       ;audition (out 0 (stkShakers AR i e sd no rf tr))}

> let tr = impulse KR 1 0 - 0.5
> in audition (out 0 (stkShakers AR 4 64 64 64 64 tr))
