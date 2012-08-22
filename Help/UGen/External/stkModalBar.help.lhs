> Sound.SC3.UGen.Help.viewSC3Help "StkModalBar"
> Sound.SC3.UGen.DB.ugenSummary "StkModalBar"

> import Control.Monad
> import Sound.SC3.ID
> import qualified Sound.SC3.Monadic as M

> let {x = mouseX KR 0.25 4 Linear 0.2
>     ;tr = impulse KR x 0 - 0.5
>     ;tR = M.tRand 0 127 tr}
> in do {i <- M.tRand 0 9 tr
>       ;mn <- M.tIRand 25 96 tr
>       ;[sh,sp,vg,vf,mx,v] <- replicateM 6 tR
>       ;let s = stkModalBar AR (midiCPS mn) i sh sp vg vf mx v tr
>        in audition (out 0 s)}

> let {x = mouseX KR 1 6 Linear 0.2
>     ;t = impulse KR x 0 - 0.5
>     ;tr = pulseDivider t 6 0}
> in do {mn <- M.tIRand 52 64 t
>       ;sh <- M.tRand 4 8 tr
>       ;sp <- M.tRand 54 68 tr
>       ;vg <- M.tRand 66 98 tr
>       ;vf <- M.tRand 4 12 tr
>       ;mx <- M.tRand 0 1 tr
>       ;v <- M.tRand 16 48 tr
>       ;let s = stkModalBar AR (midiCPS mn) 1 sh sp vg vf mx v t
>        in audition (out 0 s)}
