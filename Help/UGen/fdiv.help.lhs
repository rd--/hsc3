    > Sound.SC3.UGen.Help.viewSC3Help "Operator./"
    > :t (/)

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let o = fSinOsc KR 10 0.5
>         n = pinkNoise 'α' AR
>     in (n * 0.1) / (o * 0.75)
