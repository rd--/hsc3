> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Record.Plain.StkShakers {- hsc3-rec -}

> gr_01 = mkStkShakers stkShakersR

> gr_02 =
>     let x = mouseX KR 0.25 4 Linear 0.2
>         tr = impulse KR x 0 - 0.5
>         u = stkShakersR
>           {instr = tRand 'α' 0 23 tr
>           ,energy = tRand 'β' 0 127 tr
>           ,decay_ = tRand 'γ' 0 127 tr
>           ,objects = tRand 'δ' 0 127 tr
>           ,resfreq = tRand 'ε' 0 127 tr}
>     in mkStkShakers u

<https://ccrma.stanford.edu/software/stk/classstk_1_1Shakers.html>
