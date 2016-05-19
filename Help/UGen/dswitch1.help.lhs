    > Sound.SC3.UGen.Help.viewSC3Help "Dswitch1"
    > Sound.SC3.UGen.DB.ugenSummary "Dswitch1"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let x = mouseX KR 0 4 Linear 0.1
>         y = mouseY KR 1 15 Linear 0.1
>         t = impulse KR 3 0
>         w = dwhite 'α' dinf 20 23
>         n = dswitch1 'β' x (mce [1, 3, y, 2, w])
>         f = demand t 0 n * 30 + 340
>     in sinOsc AR f 0 * 0.1
