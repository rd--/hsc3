    Sound.SC3.UGen.Help.viewSC3Help "LFBrownNoise2"
    Sound.SC3.UGen.DB.ugenSummary "LFBrownNoise2"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 =
>   let freq = 1000
>       dev = mouseX KR 0 1 Linear 0.2
>       dist = mouseY KR 0 5 Linear 0.2
>   in lfBrownNoise2 'α' AR freq dev dist * 0.25

Use as frequency control.

> g_02 =
>   let freq = 8
>       dev = mouseX KR 0 1 Linear 0.2
>       dist = mouseY KR 0 5 Linear 0.2
>       n1:n2:n3:_ = map (\z -> lfBrownNoise2 z KR freq dev dist) ['α'..]
>       o = impulse AR (range 6 24 n1) 0
>   in lagUD o (range 0.0001 0.001 n2) (range 0.0001 0.001 n3) * 0.5
