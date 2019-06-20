> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let n = dust 'α' AR 3
>   in ringz (n * 0.3) 2000 2

> g_02 =
>   let n = whiteNoise 'α' AR
>   in ringz (n * 0.005) 2000 0.5

Modulate frequency

> g_03 =
>   let {n = whiteNoise 'α' AR
>     ;f = xLine KR 100 3000 10 RemoveSynth}
>   in ringz (n * 0.005) f 0.5

> g_04 =
>   let f = xLine KR 100 3000 10 RemoveSynth
>   in ringz (impulse AR 6 0.3) f 0.5

Modulate ring time

> g_05 =
>   let rt = xLine KR 4 0.04 8 RemoveSynth
>   in ringz (impulse AR 6 0.3) 2000 rt

Modulate ring time opposite direction

> g_06 =
>   let rt = xLine KR 0.04 4 8 RemoveSynth
>   in ringz (impulse AR 6 0.3) 2000 rt
