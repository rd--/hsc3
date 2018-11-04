    > Sound.SC3.UGen.Help.viewSC3Help "StkShakers"
    > Sound.SC3.UGen.DB.ugenSummary "StkShakers"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as Ext {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.RDU as RDU {- sc3-rdu -}

> gr_01 =
>     let x = mouseX KR 0.25 4 Linear 0.2
>         tr = impulse KR x 0 - 0.5
>         instr = tRand 'α' 0 23 tr
>         [energy,decay,objects,resfreq] = mceChannels (RDU.tRandN 4 'β' 0 127 tr)
>     in Ext.stkShakers AR instr energy decay objects resfreq

> gr_02 = Ext.stkShakers AR 4 64 64 64 64

<https://ccrma.stanford.edu/software/stk/classstk_1_1Shakers.html>
