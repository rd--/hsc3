    Sound.SC3.UGen.Help.viewSC3Help "TGaussRand"
    Sound.SC3.UGen.DB.ugenSummary "TGaussRand"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins {- hsc3 -}

> g_01 =
>     let t = dust 'α' KR 10
>         f = tGaussRand 'β' 300 3000 t
>         o = sinOsc AR f 0
>         l = tGaussRand 'γ' (-1) 1 t
>     in  pan2 o l 0.1

compare to tRand

> g_02 =
>     let t = dust 'α' KR 10
>         f = tRand 'β' 300 3000 t
>         o = sinOsc AR f 0
>         l = tRand 'γ' (-1) 1 t
>     in pan2 o l 0.1
