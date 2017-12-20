    Sound.SC3.UGen.Help.viewSC3Help "StkModalBar"
    Sound.SC3.UGen.DB.ugenSummary "StkModalBar"

> import Control.Monad
> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

requires "../../rawwaves/marmstk1.raw"

> g_01 :: UId m => m UGen
> g_01 = do
>   let x = mouseX KR 0.25 4 Linear 0.2
>       tr = impulse KR x 0 - 0.5
>       tR = tRandM 0 127 tr
>   i <- tRandM 0 9 tr
>   mn <- tIRandM 25 96 tr
>   [sh,sp,vg,vf,mx,v] <- replicateM 6 tR
>   return (stkModalBar AR (midiCPS mn) i sh sp vg vf mx v tr)

> g_02 :: UId m => m UGen
> g_02 = do
>   let x = mouseX KR 1 6 Linear 0.2
>       t = impulse KR x 0 - 0.5
>       tr = pulseDivider t 6 0
>   mn <- tIRandM 52 64 t
>   sh <- tRandM 4 8 tr
>   sp <- tRandM 54 68 tr
>   vg <- tRandM 66 98 tr
>   vf <- tRandM 4 12 tr
>   mx <- tRandM 0 1 tr
>   v <- tRandM 16 48 tr
>   return (stkModalBar AR (midiCPS mn) 1 sh sp vg vf mx v t)
