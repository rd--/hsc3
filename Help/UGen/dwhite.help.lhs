    > Sound.SC3.UGen.Help.viewSC3Help "Dwhite"
    > Sound.SC3.UGen.DB.ugenSummary "Dwhite"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let n = dwhite 'α' 30 0 15
>         x = mouseX KR 1 40 Exponential 0.1
>         t = impulse KR x 0
>         f = demand t 0 n * 30 + 340
>     in sinOsc AR f 0 * 0.1
