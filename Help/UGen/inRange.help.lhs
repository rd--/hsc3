    > Sound.SC3.UGen.Help.viewSC3Help "InRange"
    > Sound.SC3.UGen.DB.ugenSummary "InRange"

> import Sound.SC3 {- hsc3 -}

trigger noise burst

> g_01 =
>     let n = brownNoise 'α' AR * 0.1
>         x = mouseX KR 1 2 Linear 0.1
>         o = sinOsc KR x 0 * 0.2
>     in inRange o (-0.15) 0.15 * n
