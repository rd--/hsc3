    > Sound.SC3.UGen.Help.viewSC3Help "PanAz"
    > Sound.SC3.UGen.DB.ugenSummary "PanAz"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let o = pinkNoise 'α' AR
>         nc = 4
>         fr = 0.15
>     in panAz nc o (lfSaw KR fr 0) 0.1 2 0.5
