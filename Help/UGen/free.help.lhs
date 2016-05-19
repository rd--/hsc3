    > Sound.SC3.UGen.Help.viewSC3Help "Free"
    > Sound.SC3.UGen.DB.ugenSummary "Free"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = out 0 (sinOsc AR 880 0 * 0.1)
>
> g_02 =
>     let n0 = pinkNoise 'α' AR
>         n1 = dust 'β' KR 20
>     in mrg [out 1 (n0 * 0.1), free n1 1001]

    > audition_at (1001,AddToTail,1,[]) g_01
    > audition_at (-1,AddToTail,1,[]) g_02

