    Sound.SC3.UGen.Help.viewSC3Help "LFBrownNoise2"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let freq = 1000
>       dev = mouseX KR 0 1 Linear 0.2
>       dist = mouseY KR 0 5 Linear 0.2
>   in X.lfBrownNoise2 'α' AR freq dev dist * 0.25

Use as frequency control.

> g_02 =
>   let freq = 8
>       dev = mouseX KR 0 1 Linear 0.2
>       dist = mouseY KR 0 5 Linear 0.2
>       n1:n2:n3:_ = map (\z -> X.lfBrownNoise2 z KR freq dev dist) ['α'..]
>       o = impulse AR (range 6 24 n1) 0
>   in lagUD o (range 0.0001 0.001 n2) (range 0.0001 0.001 n3) * 0.5

Use as pan & volume controls (external sound input)

> f_01 s =
>   let freq = range 0.5 2 (X.lfBrownNoise2 'α' KR 2 0.1 5)
>       dev = mouseX KR 0.01 0.35 Linear 0.2
>       dist = mouseY KR 0 5 Linear 0.2
>       n1:n2:_ = map (\z -> X.lfBrownNoise2 z KR freq dev dist) ['β'..]
>   in pan2 s (range (-0.75) 0.75 n1) 1 * range 0.01 0.5 n2

> g_03 = f_01 (soundIn 0)

> g_04 = f_01 (sinOsc AR 440 0 * 0.1)
