> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let o = fSinOsc AR 1000 0
>         l = line KR 0 1 8 DoNothing
>     in fold2 o l
