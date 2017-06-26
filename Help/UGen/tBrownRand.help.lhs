    Sound.SC3.UGen.Help.viewSC3Help "TBrownRand"
    Sound.SC3.UGen.DB.ugenSummary "TBrownRand"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins {- hsc3 -}

> g_01 =
>     let t = dust 'α' KR 10
>         dist = mouseX KR 0 5 Linear 0.2
>         f = tBrownRand 'β' 300 3000 1 dist t
>     in sinOsc AR f 0 * 0.1

> g_02 =
>     let t = dust 'α' KR 10
>         n = tBrownRand 'β' 0 1 0.2 0 t
>         f = linExp n 0 1 300 3000
>         o = sinOsc AR f 0
>         l = tBrownRand 'γ' (-1) 1 1 4 t
>     in pan2 o l 0.1
