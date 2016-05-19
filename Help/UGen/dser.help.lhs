    > Sound.SC3.UGen.Help.viewSC3Help "Dser"
    > Sound.SC3.UGen.DB.ugenSummary "Dser"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let a = dser 'α' 7 (mce [1, 3, 2, 7, 8])
>         x = mouseX KR 1 40 Exponential 0.1
>         t = impulse KR x 0
>         f = demand t 0 a * 30 + 340
>     in sinOsc AR f 0 * 0.1
