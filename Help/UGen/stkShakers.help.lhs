    > Sound.SC3.UGen.Help.viewSC3Help "StkShakers"
    > Sound.SC3.UGen.DB.ugenSummary "StkShakers"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins {- hsc3 -}
> import Sound.SC3.UGen.External.RDU {- hsc3 -}

> gr_01 =
>     let x = mouseX KR 0.25 4 Linear 0.2
>         tr = impulse KR x 0 - 0.5
>         i = tRand 'α' 0 23 tr
>         [e,sd,no,rf] = mceChannels (tRandN 4 'β' 0 127 tr)
>     in stkShakers AR i e sd no rf tr

> gr_02 = stkShakers AR 4 64 64 64 64 1
