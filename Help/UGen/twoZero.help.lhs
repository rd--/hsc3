> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let n = whiteNoise 'α' AR
>         f = xLine KR 20 20000 8 RemoveSynth
>     in twoZero (n * 0.125) f 1
