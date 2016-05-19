    > Sound.SC3.UGen.Help.viewSC3Help "Dseries"
    > Sound.SC3.UGen.DB.ugenSummary "Dseries"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let n = dseries 'α' 15 0 1
>         x = mouseX KR 1 40 Exponential 0.1
>         t = impulse KR x 0
>         f = demand t 0 n * 30 + 340
>     in sinOsc AR f 0 * 0.1
