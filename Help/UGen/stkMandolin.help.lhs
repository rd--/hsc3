> Sound.SC3.UGen.Help.viewSC3Help "StkMandolin"
> Sound.SC3.UGen.DB.ugenSummary "StkMandolin"

> import Control.Monad
> import Sound.SC3

requires "../../rawwaves/mand1.raw"

> let {x = mouseX KR 0.25 4 Linear 0.2
>     ;tr = impulse KR x 0 - 0.5 }
> in do {mn <- tRandM 54 66 tr
>       ;[bs, pp, dm, dt, at] <- replicateM 5 (tRandM 0 127 tr)
>       ;audition (out 0 (stkMandolin AR (midiCPS mn) bs pp dm dt at tr))}

> let {x = mouseX KR 3 16 Linear 0.2
>     ;t = impulse KR x 0 - 0.5
>     ;tr = pulseDivider t 6 0 }
> in do {mn <- tIRandM 54 66 t
>       ;bs <- tRandM 72 94 tr
>       ;pp <- tRandM 32 42 tr
>       ;dm <- tRandM 64 72 tr
>       ;dt <- tRandM 0 4 tr
>       ;at <- tRandM 2 8 tr
>       ;audition (out 0 (stkMandolin AR (midiCPS mn) bs pp dm dt at t))}
