> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let tr = t2k (dust 'α' AR 4)
>   in trig tr 0.1 * sinOsc AR 800 0 * 0.1
