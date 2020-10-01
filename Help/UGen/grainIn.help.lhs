> import Sound.SC3 {- hsc3 -}

> f_01 s =
>     let x = mouseX KR (-0.5) 0.5 Linear 0.1
>         y = mouseY KR 5 25 Linear 0.1
>         t = impulse KR y 0
>     in grainIn 2 t 0.1 s x (-1) 512

> g_01 = f_01 (pinkNoise 'α' AR * 0.1)

> g_02 = let s = soundIn 0 in s * 0.05 + f_01 s
