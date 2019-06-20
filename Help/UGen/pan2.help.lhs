> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let n = pinkNoise 'α' AR
>     in pan2 n (fSinOsc KR 2 0) 0.3

> g_02 =
>     let n = pinkNoise 'α' AR
>         x = mouseX KR (-1) 1 Linear 0.2
>         y = mouseY KR 0 1 Linear 0.2
>     in pan2 n x y
