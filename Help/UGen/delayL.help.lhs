See delayN

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let t = mouseX KR 0.001 0.2 Exponential 0.2
>       s = impulse AR 1 0
>       d = delayL s 0.6 t
>   in mce2 d s

