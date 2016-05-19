    > Sound.SC3.UGen.Help.viewSC3Help "LinPan2"
    > Sound.SC3.UGen.DB.ugenSummary "LinPan2"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let n = pinkNoise 'α' AR
>     in linPan2 n (fSinOsc KR 2 0) 0.1
>
> g_02 = linPan2 (fSinOsc AR 800 0) (fSinOsc KR 3 0) 0.1
