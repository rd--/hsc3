> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let d = dust 'α' AR 1
>         o = fSinOsc AR 800 0 * 0.5
>     in o * trig d 0.2
